# Microsoft Developer Studio Project File - Name="colander" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=colander - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Colander.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Colander.mak" CFG="colander - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "colander - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "colander - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Andes2/Algebra/Colander", GSLAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "colander - Win32 Release"

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

!ELSEIF  "$(CFG)" == "colander - Win32 Debug"

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

# Name "colander - Win32 Release"
# Name "colander - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src\checkeqs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\cleanup.cpp
# End Source File
# Begin Source File

SOURCE=..\src\colander.cpp
# End Source File
# Begin Source File

SOURCE=..\src\copyexpr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\desperate.cpp
# End Source File
# Begin Source File

SOURCE=..\src\despquad.cpp
# End Source File
# Begin Source File

SOURCE=..\src\despquadb.cpp
# End Source File
# Begin Source File

SOURCE=..\src\dimenchk.cpp
# End Source File
# Begin Source File

SOURCE=..\src\dimens.cpp
# End Source File
# Begin Source File

SOURCE=..\src\distfrac.cpp
# End Source File
# Begin Source File

SOURCE=..\src\dofactor.cpp
# End Source File
# Begin Source File

SOURCE=..\src\donlsolv.cpp
# End Source File
# Begin Source File

SOURCE=..\src\dopurelin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\dotrig.cpp
# End Source File
# Begin Source File

SOURCE=..\src\eqnumsimp.cpp
# End Source File
# Begin Source File

SOURCE=..\src\equaleqs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\expr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\exprp.cpp
# End Source File
# Begin Source File

SOURCE=..\src\factorout.cpp
# End Source File
# Begin Source File

SOURCE=..\src\fixupforpls.cpp
# End Source File
# Begin Source File

SOURCE=..\src\flatten.cpp
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

SOURCE=..\src\ispos.cpp
# End Source File
# Begin Source File

SOURCE=..\src\justonev.cpp
# End Source File
# Begin Source File

SOURCE=..\src\lookslikeint.cpp
# End Source File
# Begin Source File

SOURCE=..\src\moreexpr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\multsort.cpp
# End Source File
# Begin Source File

SOURCE=..\src\nlsolvov.cpp
# End Source File
# Begin Source File

SOURCE=..\src\normexpr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\numfactorsof.cpp
# End Source File
# Begin Source File

SOURCE=..\src\numunknowns.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ordinvars.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ordunknowns.cpp
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

SOURCE=..\src\plussort.cpp
# End Source File
# Begin Source File

SOURCE=..\src\polysolve.cpp
# End Source File
# Begin Source File

SOURCE=..\src\powonev.cpp
# End Source File
# Begin Source File

SOURCE=..\src\purelin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\qsrtexpr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\rationalize.cpp
# End Source File
# Begin Source File

SOURCE=..\src\recassign.cpp
# End Source File
# Begin Source File

SOURCE=..\src\slvlinonev.cpp
# End Source File
# Begin Source File

SOURCE=..\src\solveknownvar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\solvetrig.cpp
# End Source File
# Begin Source File

SOURCE=..\src\solvetrigb.cpp
# End Source File
# Begin Source File

SOURCE=..\src\subexpin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\substin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\treechk.cpp
# End Source File
# Begin Source File

SOURCE=..\src\trigsimp.cpp
# End Source File
# Begin Source File

SOURCE=..\src\unitabr.cpp
# End Source File
# Begin Source File

SOURCE=..\src\utils.cpp
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

SOURCE=..\src\mconst.h
# End Source File
# Begin Source File

SOURCE=..\src\standard.h
# End Source File
# Begin Source File

SOURCE=..\src\unitabr.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
