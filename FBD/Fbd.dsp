# Microsoft Developer Studio Project File - Name="FBD" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=FBD - Win32 DLL Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Fbd.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Fbd.mak" CFG="FBD - Win32 DLL Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "FBD - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Atlas Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Atlas Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 TCP Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 TCP Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 WOZ Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 WOZ Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Roving Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Roving Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Physics Lite Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 OLI Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 OLI Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 DLL Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 DLL Release" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/FBD", BAAAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "FBD - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Release"
# PROP Intermediate_Dir ".\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /Yu"stdafx.h" /c
# ADD CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\Debug"
# PROP BASE Intermediate_Dir ".\Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Debug"
# PROP Intermediate_Dir ".\Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /Yu"stdafx.h" /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "FBD___Win32_Atlas_Debug"
# PROP BASE Intermediate_Dir "FBD___Win32_Atlas_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\Atlas_Debug"
# PROP Intermediate_Dir ".\Atlas_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fr /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT BASE LINK32 /profile
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "FBD___Win32_Atlas_Release"
# PROP BASE Intermediate_Dir "FBD___Win32_Atlas_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\Atlas_Release"
# PROP Intermediate_Dir ".\Atlas_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O1 /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "FBD___Win32_TCP_Debug"
# PROP BASE Intermediate_Dir "FBD___Win32_TCP_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\TCP_Debug"
# PROP Intermediate_Dir ".\TCP_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fr /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT BASE LINK32 /profile
# ADD LINK32 winmm.lib version.lib wininet.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Debug/Fbd-tcp.exe"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "FBD___Win32_TCP_Release"
# PROP BASE Intermediate_Dir "FBD___Win32_TCP_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\TCP_Release"
# PROP Intermediate_Dir ".\TCP_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O1 /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Release/Fbd-tcp.exe"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
OutDir=.\TCP_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent directory
PostBuild_Cmds=copy $(OutDir)\fbd-tcp.exe ..
# End Special Build Tool

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "FBD___Win32_WOZ_Debug"
# PROP BASE Intermediate_Dir "FBD___Win32_WOZ_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WOZ_Debug"
# PROP Intermediate_Dir "WOZ_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Debug/Fbd-woz.exe"
# SUBTRACT BASE LINK32 /profile
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\WOZ_Debug/Fbd-woz.exe"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "FBD___Win32_WOZ_Release"
# PROP BASE Intermediate_Dir "FBD___Win32_WOZ_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WOZ_Release"
# PROP Intermediate_Dir "WOZ_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Release/Fbd-woz.exe"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\WOZ_Release/Fbd-woz.exe"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "FBD___Win32_Roving_Release"
# PROP BASE Intermediate_Dir "FBD___Win32_Roving_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Roving_Release"
# PROP Intermediate_Dir "Roving_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Release/Fbd-tcp.exe"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 winmm.lib version.lib wininet.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\Roving_Release/Fbd-net.exe"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
TargetPath=.\Roving_Release\Fbd-net.exe
SOURCE="$(InputPath)"
PostBuild_Desc=copying $(TargetPath) into C:\Andes2
PostBuild_Cmds=copy $(TargetPath) C:\Andes2
# End Special Build Tool

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "FBD___Win32_Roving_Debug"
# PROP BASE Intermediate_Dir "FBD___Win32_Roving_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Roving_Debug"
# PROP Intermediate_Dir "Roving_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Debug/Fbd-tcp.exe"
# SUBTRACT BASE LINK32 /profile
# ADD LINK32 winmm.lib version.lib wininet.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\Roving_Debug/Fbd-net.exe"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "FBD___Win32_Physics_Lite_Release"
# PROP BASE Intermediate_Dir "FBD___Win32_Physics_Lite_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Lite_Release"
# PROP Intermediate_Dir "Lite_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Release/Fbd-tcp.exe"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\Lite_Release/Fbd-lite.exe"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
OutDir=.\Lite_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into C:\Andes2
PostBuild_Cmds=copy $(OutDir)\fbd-lite.exe C:\Andes2
# End Special Build Tool

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "FBD___Win32_OLI_Release"
# PROP BASE Intermediate_Dir "FBD___Win32_OLI_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "OLI_Release"
# PROP Intermediate_Dir "OLI_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Release/Fbd-tcp.exe"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 winmm.lib version.lib ../helpifc.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\OLI_Release/Fbd-tcp.exe"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
OutDir=.\OLI_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent dir
PostBuild_Cmds=copy $(OutDir)\fbd-tcp.exe ..\fbd-tcp-oli.exe
# End Special Build Tool

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "FBD___Win32_OLI_Debug"
# PROP BASE Intermediate_Dir "FBD___Win32_OLI_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "OLI_Debug"
# PROP Intermediate_Dir "OLI_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib wininet.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Debug/Fbd-tcp.exe"
# SUBTRACT BASE LINK32 /profile
# ADD LINK32 wininet.lib winmm.lib version.lib ../helpifc.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\OLI_Debug/Fbd-tcp.exe"
# SUBTRACT LINK32 /profile

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "FBD___Win32_DLL_Debug"
# PROP BASE Intermediate_Dir "FBD___Win32_DLL_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ".\DLL_Debug"
# PROP Intermediate_Dir ".\DLL_Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib wininet.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Debug/Fbd-tcp.exe"
# SUBTRACT BASE LINK32 /profile
# ADD LINK32 winmm.lib version.lib wininet.lib ../helpifc.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\Dll_Debug/Fbd-dll.exe"
# SUBTRACT LINK32 /profile
# Begin Special Build Tool
OutDir=.\DLL_Debug
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent directory
PostBuild_Cmds=copy $(OutDir)\fbd-dll.exe ..
# End Special Build Tool

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "FBD___Win32_DLL_Release"
# PROP BASE Intermediate_Dir "FBD___Win32_DLL_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 6
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ".\DLL_Release"
# PROP Intermediate_Dir ".\DLL_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 winmm.lib version.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\TCP_Release/Fbd-tcp.exe"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 winmm.lib version.lib wininet.lib ../helpifc.lib /nologo /subsystem:windows /debug /machine:I386 /out:".\DLL_Release/Fbd-dll.exe"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
OutDir=.\DLL_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent directory
PostBuild_Cmds=copy $(OutDir)\fbd-dll.exe ..
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "FBD - Win32 Release"
# Name "FBD - Win32 Debug"
# Name "FBD - Win32 Atlas Debug"
# Name "FBD - Win32 Atlas Release"
# Name "FBD - Win32 TCP Debug"
# Name "FBD - Win32 TCP Release"
# Name "FBD - Win32 WOZ Debug"
# Name "FBD - Win32 WOZ Release"
# Name "FBD - Win32 Roving Release"
# Name "FBD - Win32 Roving Debug"
# Name "FBD - Win32 Physics Lite Release"
# Name "FBD - Win32 OLI Release"
# Name "FBD - Win32 OLI Debug"
# Name "FBD - Win32 DLL Debug"
# Name "FBD - Win32 DLL Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Group "Global objs (History, HelpIfc)"

# PROP Default_Filter ""
# Begin Source File

SOURCE=".\helpifc-dll.cpp"

!IF  "$(CFG)" == "FBD - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

# PROP BASE Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=".\helpifc-tcp.cpp"

!IF  "$(CFG)" == "FBD - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

# PROP Intermediate_Dir ".\DLL_Debug"
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\HelpIfc.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\history.cpp
# End Source File
# End Group
# Begin Source File

SOURCE=.\AngleDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\AreaDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\AuthDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\AxesDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Base64Coder.cpp
# SUBTRACT CPP /YX /Yc /Yu
# End Source File
# Begin Source File

SOURCE=.\BrowsDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ChatView.cpp
# End Source File
# Begin Source File

SOURCE=.\CheckedDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Childfrm.cpp
# End Source File
# Begin Source File

SOURCE=.\ChoiceDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\CommentDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\CurrentDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\DemoDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\DipoleDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\DocManagerEx.cpp
# End Source File
# Begin Source File

SOURCE=.\DocPages.cpp
# End Source File
# Begin Source File

SOURCE=.\Example\DoneDlg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\DrawObj.cpp
# End Source File
# Begin Source File

SOURCE=.\DrawObjDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\EnergyDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\EQEdit.cpp
# End Source File
# Begin Source File

SOURCE=.\EQView.cpp
# End Source File
# Begin Source File

SOURCE=.\Example\exhintdg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\Exp2Dlg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\Exp4Dlg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\expbdydg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\EXPlanVw.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\ExpLawDlg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\expmenu.rc

!IF  "$(CFG)" == "FBD - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Example\extxtdlg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\exview.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\FBD.cpp
# End Source File
# Begin Source File

SOURCE=.\FBDDoc.cpp
# End Source File
# Begin Source File

SOURCE=.\FBDObj.cpp
# End Source File
# Begin Source File

SOURCE=.\fbdview.cpp
# End Source File
# Begin Source File

SOURCE=.\FieldDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\FormulaDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\GreekOpts.cpp
# End Source File
# Begin Source File

SOURCE=.\GridCtrl.cpp
# End Source File
# Begin Source File

SOURCE=.\GridDropTarget.cpp
# End Source File
# Begin Source File

SOURCE=.\res\grkbmp.bmp
# End Source File
# Begin Source File

SOURCE=.\HelpFilesPage.cpp
# End Source File
# Begin Source File

SOURCE=.\HiLevelVw.cpp
# End Source File
# Begin Source File

SOURCE=.\HintDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\HintView.cpp
# End Source File
# Begin Source File

SOURCE=.\HlpOpDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\HypertxtDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ImpulseDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\InPlaceList.cpp
# End Source File
# Begin Source File

SOURCE=.\ItemCtrl.cpp
# End Source File
# Begin Source File

SOURCE=.\LabelDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\LabRadDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\LawDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\LineDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\LispReader.cpp
# End Source File
# Begin Source File

SOURCE=.\listvwex.cpp
# End Source File
# Begin Source File

SOURCE=.\LogEdit.cpp
# End Source File
# Begin Source File

SOURCE=.\LoginDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\MainFrm.cpp
# End Source File
# Begin Source File

SOURCE=.\MCQDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\MDIFixedSizeFrm.cpp
# End Source File
# Begin Source File

SOURCE=.\messages.cpp
# End Source File
# Begin Source File

SOURCE=.\MotDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Motion.cpp
# End Source File
# Begin Source File

SOURCE=.\MyGrids.cpp
# End Source File
# Begin Source File

SOURCE=.\MyMenu.cpp
# End Source File
# Begin Source File

SOURCE=.\OliView.cpp
# End Source File
# Begin Source File

SOURCE=.\PicCtrl.cpp
# End Source File
# Begin Source File

SOURCE=.\PictCtrl.cpp
# End Source File
# Begin Source File

SOURCE=.\Picture.cpp
# End Source File
# Begin Source File

SOURCE=.\Example\planobj.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Example\planstrs.cpp
# ADD CPP /I "."
# SUBTRACT CPP /YX /Yc /Yu
# End Source File
# Begin Source File

SOURCE=.\Example\planview.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\Playdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\PopupWnd.cpp
# End Source File
# Begin Source File

SOURCE=.\PrincView.cpp
# End Source File
# Begin Source File

SOURCE=.\ProbDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ProblemSet.cpp
# End Source File
# Begin Source File

SOURCE=.\ProbSetEditView.cpp
# End Source File
# Begin Source File

SOURCE=.\ProbSetView.cpp
# End Source File
# Begin Source File

SOURCE=.\PropertyDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\PsmDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\PtrDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\RecordScreen.cpp
# End Source File
# Begin Source File

SOURCE=.\Rectdlg.cpp
# End Source File
# Begin Source File

SOURCE=.\RelVelDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ResistanceDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\RichCombo.cpp
# End Source File
# Begin Source File

SOURCE=.\RichEditEx.cpp
# End Source File
# Begin Source File

SOURCE=.\RuleQDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\SolveVarDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Splash.cpp
# End Source File
# Begin Source File

SOURCE=.\StageObj.cpp
# End Source File
# Begin Source File

SOURCE=.\StatLink.cpp
# End Source File
# Begin Source File

SOURCE=.\Stdafx.cpp
# ADD CPP /Yc"stdafx.h"
# End Source File
# Begin Source File

SOURCE=.\Example\stepdlg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\SymbolMenu.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\SysDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\TabView.cpp
# End Source File
# Begin Source File

SOURCE=.\TaskDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Example\TemplateDlg.cpp
# ADD CPP /I "."
# End Source File
# Begin Source File

SOURCE=.\TimeConstantDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\TorqueDipoleDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\TorqueDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\TraceDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\TransferDlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\UnitVectorDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ValueDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VariableDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VarView.cpp
# End Source File
# Begin Source File

SOURCE=.\VecAVDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VecCpDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VecDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VecPosDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VideoDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VoltageDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\VwOptDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\wbrowser.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Group "History, HelpIfc"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\Helpifc.h
# End Source File
# Begin Source File

SOURCE=.\History.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\AngleDlg.h
# End Source File
# Begin Source File

SOURCE=.\AreaDlg.h
# End Source File
# Begin Source File

SOURCE=.\AuthDlg.h
# End Source File
# Begin Source File

SOURCE=.\AxesDlg.h
# End Source File
# Begin Source File

SOURCE=.\Base64Coder.h
# End Source File
# Begin Source File

SOURCE=.\Browsdlg.h
# End Source File
# Begin Source File

SOURCE=.\CellRange.h
# End Source File
# Begin Source File

SOURCE=.\ChatView.h
# End Source File
# Begin Source File

SOURCE=.\childfrm.h
# End Source File
# Begin Source File

SOURCE=.\ChoiceDlg.h
# End Source File
# Begin Source File

SOURCE=.\CommentDlg.h
# End Source File
# Begin Source File

SOURCE=.\CurrentDlg.h
# End Source File
# Begin Source File

SOURCE=.\DemoDlg.h
# End Source File
# Begin Source File

SOURCE=.\DipoleDlg.h
# End Source File
# Begin Source File

SOURCE=.\DocManagerEx.h
# End Source File
# Begin Source File

SOURCE=.\Docpages.h
# End Source File
# Begin Source File

SOURCE=.\Example\DoneDlg.h
# End Source File
# Begin Source File

SOURCE=.\DrawObj.h
# End Source File
# Begin Source File

SOURCE=.\DrawObjDlg.h
# End Source File
# Begin Source File

SOURCE=.\EnergyDlg.h
# End Source File
# Begin Source File

SOURCE=.\EQEdit.h
# End Source File
# Begin Source File

SOURCE=.\Eqview.h
# End Source File
# Begin Source File

SOURCE=.\example\EXHintDg.h
# End Source File
# Begin Source File

SOURCE=.\Example\Exp2Dlg.h
# End Source File
# Begin Source File

SOURCE=.\Example\Exp4Dlg.h
# End Source File
# Begin Source File

SOURCE=.\Example\expbdydg.h
# End Source File
# Begin Source File

SOURCE=.\Example\EXPlanVw.h
# End Source File
# Begin Source File

SOURCE=.\Example\ExpLawDlg.h
# End Source File
# Begin Source File

SOURCE=.\Example\expmenu.h
# End Source File
# Begin Source File

SOURCE=.\Example\extxtdlg.h
# End Source File
# Begin Source File

SOURCE=.\Example\exview.h
# End Source File
# Begin Source File

SOURCE=.\Fbd.h
# End Source File
# Begin Source File

SOURCE=.\fbddoc.h
# End Source File
# Begin Source File

SOURCE=.\FBDObj.h
# End Source File
# Begin Source File

SOURCE=.\Fbdview.h
# End Source File
# Begin Source File

SOURCE=.\FieldDlg.h
# End Source File
# Begin Source File

SOURCE=.\FormulaDlg.h
# End Source File
# Begin Source File

SOURCE=.\GreekOpts.h
# End Source File
# Begin Source File

SOURCE=.\GridCtrl.h
# End Source File
# Begin Source File

SOURCE=.\GridDropTarget.h
# End Source File
# Begin Source File

SOURCE=.\HelpFilesPage.h
# End Source File
# Begin Source File

SOURCE=.\HiLevelVw.h
# End Source File
# Begin Source File

SOURCE=.\HintDlg.h
# End Source File
# Begin Source File

SOURCE=.\HintView.h
# End Source File
# Begin Source File

SOURCE=.\Hlpopdlg.h
# End Source File
# Begin Source File

SOURCE=.\HypertxtDlg.h
# End Source File
# Begin Source File

SOURCE=.\ImpulseDlg.h
# End Source File
# Begin Source File

SOURCE=.\InPlaceList.h
# End Source File
# Begin Source File

SOURCE=.\ItemCtrl.h
# End Source File
# Begin Source File

SOURCE=.\Labeldlg.h
# End Source File
# Begin Source File

SOURCE=.\LabRadDlg.h
# End Source File
# Begin Source File

SOURCE=.\LawDialog.h
# End Source File
# Begin Source File

SOURCE=.\Lgdialog.h
# End Source File
# Begin Source File

SOURCE=.\LineDlg.h
# End Source File
# Begin Source File

SOURCE=.\LispReader.h
# End Source File
# Begin Source File

SOURCE=.\listvwex.h
# End Source File
# Begin Source File

SOURCE=.\Logedit.h
# End Source File
# Begin Source File

SOURCE=.\LoginDlg.h
# End Source File
# Begin Source File

SOURCE=.\Mainfrm.h
# End Source File
# Begin Source File

SOURCE=.\MCQDlg.h
# End Source File
# Begin Source File

SOURCE=.\MDIFixedSizeFrm.h
# End Source File
# Begin Source File

SOURCE=.\MemDC.h
# End Source File
# Begin Source File

SOURCE=.\messages.h
# End Source File
# Begin Source File

SOURCE=.\MotDlg.h
# End Source File
# Begin Source File

SOURCE=.\Motion.h
# End Source File
# Begin Source File

SOURCE=.\MyGrids.h
# End Source File
# Begin Source File

SOURCE=.\MyMenu.h
# End Source File
# Begin Source File

SOURCE=.\OliView.h
# End Source File
# Begin Source File

SOURCE=.\PicCtrl.h
# End Source File
# Begin Source File

SOURCE=.\PictCtrl.h
# End Source File
# Begin Source File

SOURCE=.\Picture.h
# End Source File
# Begin Source File

SOURCE=.\Example\planobj.h
# End Source File
# Begin Source File

SOURCE=.\Example\planstrs.h
# End Source File
# Begin Source File

SOURCE=.\Example\planview.h
# End Source File
# Begin Source File

SOURCE=.\Playdlg.h
# End Source File
# Begin Source File

SOURCE=.\PopupWnd.h
# End Source File
# Begin Source File

SOURCE=.\PrincView.h
# End Source File
# Begin Source File

SOURCE=.\ProbDlg.h
# End Source File
# Begin Source File

SOURCE=.\ProblemSet.h
# End Source File
# Begin Source File

SOURCE=.\ProbSetEditView.h
# End Source File
# Begin Source File

SOURCE=.\ProbSetView.h
# End Source File
# Begin Source File

SOURCE=.\PropertyDlg.h
# End Source File
# Begin Source File

SOURCE=.\PsmDlg.h
# End Source File
# Begin Source File

SOURCE=.\PtrDlg.h
# End Source File
# Begin Source File

SOURCE=.\Rectdlg.h
# End Source File
# Begin Source File

SOURCE=.\RelVelDlg.h
# End Source File
# Begin Source File

SOURCE=.\ResistanceDlg.h
# End Source File
# Begin Source File

SOURCE=.\resource.h
# End Source File
# Begin Source File

SOURCE=.\RichCombo.h
# End Source File
# Begin Source File

SOURCE=.\RichEditEx.h
# End Source File
# Begin Source File

SOURCE=.\RuleQDlg.h
# End Source File
# Begin Source File

SOURCE=.\SolveVarDlg.h
# End Source File
# Begin Source File

SOURCE=.\Splash.h
# End Source File
# Begin Source File

SOURCE=.\StageObj.h
# End Source File
# Begin Source File

SOURCE=.\StatLink.h
# End Source File
# Begin Source File

SOURCE=.\Stdafx.h
# End Source File
# Begin Source File

SOURCE=.\Example\stepdlg.h
# End Source File
# Begin Source File

SOURCE=.\SymbolMenu.h
# End Source File
# Begin Source File

SOURCE=.\Sysdlg.h
# End Source File
# Begin Source File

SOURCE=.\TabView.h
# End Source File
# Begin Source File

SOURCE=.\TaskDlg.h
# End Source File
# Begin Source File

SOURCE=.\Example\TemplateDlg.h
# End Source File
# Begin Source File

SOURCE=.\TimeConstantDlg.h
# End Source File
# Begin Source File

SOURCE=.\TorqueDipoleDlg.h
# End Source File
# Begin Source File

SOURCE=.\TorqueDlg.h
# End Source File
# Begin Source File

SOURCE=.\TraceDlg.h
# End Source File
# Begin Source File

SOURCE=.\TransferDlg.h
# End Source File
# Begin Source File

SOURCE=.\UnitVectorDlg.h
# End Source File
# Begin Source File

SOURCE=.\ValueDlg.h
# End Source File
# Begin Source File

SOURCE=.\VariableDlg.h
# End Source File
# Begin Source File

SOURCE=.\VarView.h
# End Source File
# Begin Source File

SOURCE=.\VecAVDlg.h
# End Source File
# Begin Source File

SOURCE=.\VecCpDlg.h
# End Source File
# Begin Source File

SOURCE=.\Vecdlg.h
# End Source File
# Begin Source File

SOURCE=.\VecPosDlg.h
# End Source File
# Begin Source File

SOURCE=.\VideoDlg.h
# End Source File
# Begin Source File

SOURCE=.\VoltageDlg.h
# End Source File
# Begin Source File

SOURCE=.\Vwoptdlg.h
# End Source File
# Begin Source File

SOURCE=.\Wbrowser.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\AndesSplash4.bmp
# End Source File
# Begin Source File

SOURCE=.\AndesSplash5.jpg
# End Source File
# Begin Source File

SOURCE=.\res\angle.ico
# End Source File
# Begin Source File

SOURCE=.\res\Atlas.ico
# End Source File
# Begin Source File

SOURCE=.\AtlasSplash.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bitmap2.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bmp00001.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bmp00002.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bmp00003.bmp
# End Source File
# Begin Source File

SOURCE=.\res\checks.bmp
# End Source File
# Begin Source File

SOURCE=.\res\cur00001.cur
# End Source File
# Begin Source File

SOURCE=.\res\cursor1.cur
# End Source File
# Begin Source File

SOURCE=.\res\dipoleto.bmp
# End Source File
# Begin Source File

SOURCE=.\res\drawacc.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawangle.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawaxes.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawbody.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawcomp.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawdisp.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawforce.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawmome.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawradius.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawrelp.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawtorq.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawvel.cur
# End Source File
# Begin Source File

SOURCE=.\res\drawvelo.cur
# End Source File
# Begin Source File

SOURCE=.\EXDoc.ico
# End Source File
# Begin Source File

SOURCE=.\res\EXDoc.ico
# End Source File
# Begin Source File

SOURCE=.\res\exmplbar.bmp
# End Source File
# Begin Source File

SOURCE=.\Res\Fbd.ico
# End Source File
# Begin Source File

SOURCE=.\FBD.rc
# End Source File
# Begin Source File

SOURCE=.\Res\Fbd.rc2
# End Source File
# Begin Source File

SOURCE=.\Res\Fbddoc.ico
# End Source File
# Begin Source File

SOURCE=.\res\gosign.ico
# End Source File
# Begin Source File

SOURCE=.\res\greek.bmp
# End Source File
# Begin Source File

SOURCE=.\res\H_arrow2.cur
# End Source File
# Begin Source File

SOURCE=.\res\icon1.ico
# End Source File
# Begin Source File

SOURCE=.\Res\image.bmp
# End Source File
# Begin Source File

SOURCE=.\res\mainfram.bmp
# End Source File
# Begin Source File

SOURCE=.\res\mainfrm_exp.bmp
# End Source File
# Begin Source File

SOURCE=".\ONR-logo.jpg"
# End Source File
# Begin Source File

SOURCE=.\res\onr.bmp
# End Source File
# Begin Source File

SOURCE=.\res\opticsba.bmp
# End Source File
# Begin Source File

SOURCE=.\res\pointer.ico
# End Source File
# Begin Source File

SOURCE=.\res\probset.ico
# End Source File
# Begin Source File

SOURCE=.\res\probsete.ico
# End Source File
# Begin Source File

SOURCE=.\res\qmark.bmp
# End Source File
# Begin Source File

SOURCE=.\res\qualtool.bmp
# End Source File
# Begin Source File

SOURCE=.\res\quantbar.bmp
# End Source File
# Begin Source File

SOURCE=.\res\rotkinto.bmp
# End Source File
# Begin Source File

SOURCE=.\Splsh16.bmp
# End Source File
# Begin Source File

SOURCE=.\res\status.bmp
# End Source File
# Begin Source File

SOURCE=.\Res\Toolbar.bmp
# End Source File
# Begin Source File

SOURCE=.\Res\Toolbar1.bmp
# End Source File
# Begin Source File

SOURCE=.\res\toolbar2.bmp
# End Source File
# Begin Source File

SOURCE=.\res\winhelp.ico
# End Source File
# Begin Source File

SOURCE=.\res\zaccel_i.cur
# End Source File
# Begin Source File

SOURCE=.\res\zaccel_o.cur
# End Source File
# Begin Source File

SOURCE=.\res\zdisp_i.cur
# End Source File
# Begin Source File

SOURCE=.\res\zdisp_o.cur
# End Source File
# Begin Source File

SOURCE=.\res\zmom_i.cur
# End Source File
# Begin Source File

SOURCE=.\res\zmom_o.cur
# End Source File
# Begin Source File

SOURCE=.\res\ztor_i.cur
# End Source File
# Begin Source File

SOURCE=.\res\ztor_o.cur
# End Source File
# Begin Source File

SOURCE=.\res\zvel_i.cur
# End Source File
# Begin Source File

SOURCE=.\res\zvel_o.cur
# End Source File
# End Group
# End Target
# End Project
# Section OLE Controls
# 	{EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}
# End Section
# Section FBD : {EAB22AC1-30C1-11CF-A7EB-0000C05BAE0B}
# 	2:5:Class:CWebBrowser
# 	2:10:HeaderFile:webbrowser.h
# 	2:8:ImplFile:webbrowser.cpp
# End Section
# Section FBD : {2B6C9472-6704-11CF-BC04-0000C037C67D}
# 	1:17:ID_INDICATOR_TIME:105
# 	2:2:BH:
# 	2:17:ID_INDICATOR_TIME:ID_INDICATOR_TIME
# End Section
# Section FBD : {EAB22AC3-30C1-11CF-A7EB-0000C05BAE0B}
# 	0:14:WebBrowser.cpp:C:\Msdev\Projects\Fbd\WebBrowser.cpp
# 	0:12:WebBrowser.h:C:\Msdev\Projects\Fbd\WebBrowser.h
# 	2:21:DefaultSinkHeaderFile:webbrowser.h
# 	2:16:DefaultSinkClass:CWebBrowser
# End Section
# Section FBD : {2B6C9470-6704-11CF-BC04-0000C037C67D}
# 	0:8:Splash.h:C:\Msdev\Projects\Fbd\Splash.h
# 	0:10:Splash.cpp:C:\Msdev\Projects\Fbd\Splash.cpp
# 	1:10:IDB_SPLASH:117
# 	2:10:ResHdrName:resource.h
# 	2:11:ProjHdrName:stdafx.h
# 	2:10:WrapperDef:_SPLASH_SCRN_
# 	2:12:SplClassName:CSplashWnd
# 	2:21:SplashScreenInsertKey:4.0
# 	2:10:HeaderName:Splash.h
# 	2:10:ImplemName:Splash.cpp
# 	2:7:BmpID16:IDB_SPLASH
# End Section
