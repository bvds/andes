# Microsoft Developer Studio Project File - Name="indy" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=indy - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "indy.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "indy.mak" CFG="indy - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "indy - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "indy - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Andes2/Algebra/indy", ISLAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "indy - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "indy - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /D "UNITENABLE" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "indy - Win32 Release"
# Name "indy - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src\copyexpr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\dimenchk.cpp
# End Source File
# Begin Source File

SOURCE=..\src\dimens.cpp
# End Source File
# Begin Source File

SOURCE=..\src\expr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\exprp.cpp
# End Source File
# Begin Source File

SOURCE=..\src\getaline.cpp
# End Source File
# Begin Source File

SOURCE=..\src\getall.cpp
# End Source File
# Begin Source File

SOURCE=..\src\getallfile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\getaneqwu.cpp
# End Source File
# Begin Source File

SOURCE=..\src\getavar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\geteqs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\getnewsols.cpp
# End Source File
# Begin Source File

SOURCE=..\src\indymain.cpp
# End Source File
# Begin Source File

SOURCE=..\src\indyset.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lookslikeint.cpp
# End Source File
# Begin Source File

SOURCE=..\src\parse.cpp
# End Source File
# Begin Source File

SOURCE=..\src\parseclipseq.cpp
# End Source File
# Begin Source File

SOURCE=..\src\parseeqwunits.cpp
# End Source File
# Begin Source File

SOURCE=..\src\physconsts.cpp
# End Source File
# Begin Source File

SOURCE=..\src\physvar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\unitabr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\utils.cpp
# End Source File
# Begin Source File

SOURCE=..\src\valander.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=..\src\dbg.h
# End Source File
# Begin Source File

SOURCE=..\src\decl.h
# End Source File
# Begin Source File

SOURCE=..\src\dimens.h
# End Source File
# Begin Source File

SOURCE=..\src\expr.h
# End Source File
# Begin Source File

SOURCE=..\src\extoper.h
# End Source File
# Begin Source File

SOURCE=..\src\extstruct.h
# End Source File
# Begin Source File

SOURCE=..\src\indyset.h
# End Source File
# Begin Source File

SOURCE=..\src\mconst.h
# End Source File
# Begin Source File

SOURCE=..\src\standard.h
# End Source File
# Begin Source File

SOURCE=..\src\unitabr.h
# End Source File
# Begin Source File

SOURCE=..\src\valander.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
