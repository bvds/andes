# Microsoft Developer Studio Project File - Name="Solver" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=Solver - Win32 TraceOutput
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Solver.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Solver.mak" CFG="Solver - Win32 TraceOutput"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Solver - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Solver - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Solver - Win32 TraceOutput" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Andes2/Algebra/Solver", RYLAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Solver - Win32 Release"

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
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "SOLVER_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "SOLVER_EXPORTS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386

!ELSEIF  "$(CFG)" == "Solver - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "SOLVER_EXPORTS" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "SOLVER_EXPORTS" /D "UNITENABLE" /D "AW_POW_FIX" /D "AW_EXP" /YX /FD /I /src/" /GZ " /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept

!ELSEIF  "$(CFG)" == "Solver - Win32 TraceOutput"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Solver___Win32_TraceOutput"
# PROP BASE Intermediate_Dir "Solver___Win32_TraceOutput"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Solver_Win32_TraceOutput"
# PROP Intermediate_Dir "Solver_Win32_TraceOutput"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "SOLVER_EXPORTS" /D "UNITENABLE" /YX /FD /I /src/" /GZ " /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "SOLVER_EXPORTS" /D "UNITENABLE" /D "WITHDBG" /D "TRACE_OUTPUT" /D "AW_POW_FIX" /D "AW_EXP" /YX /FD /I /src/" /GZ " /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"Solver_Win32_TraceOutput/SolverTrace.dll" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Solver - Win32 Release"
# Name "Solver - Win32 Debug"
# Name "Solver - Win32 TraceOutput"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\src\checkeqs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\checksol.cpp
# End Source File
# Begin Source File

SOURCE=..\src\cleanup.cpp
# End Source File
# Begin Source File

SOURCE=..\src\coldriver.cpp
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

SOURCE=..\src\dimchkeqf.cpp
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

SOURCE=..\src\eqnokay.cpp
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

SOURCE=..\src\indyset.cpp
# End Source File
# Begin Source File

SOURCE=..\src\indysgg.cpp
# End Source File
# Begin Source File

SOURCE=..\src\indysgg2.cpp
# End Source File
# Begin Source File

SOURCE=..\src\indysgg3.cpp
# End Source File
# Begin Source File

SOURCE=..\src\interact.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ispos.cpp
# End Source File
# Begin Source File

SOURCE=..\src\justonev.cpp
# End Source File
# Begin Source File

SOURCE=..\src\justsolve.cpp
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

SOURCE=..\src\newindy.cpp
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

SOURCE=..\src\parseeqwunits.cpp
# End Source File
# Begin Source File

SOURCE=..\src\parseunit.cpp
# End Source File
# Begin Source File

SOURCE=..\src\physconsts.cpp
# End Source File
# Begin Source File

SOURCE=..\src\physvar.cpp
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

SOURCE=.\Solver.cpp
# End Source File
# Begin Source File

SOURCE=..\src\solvetool.cpp
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
# Begin Source File

SOURCE=..\src\valander.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\dllsupport.h
# End Source File
# Begin Source File

SOURCE=..\src\indysgg.h
# End Source File
# Begin Source File

SOURCE=.\lrdcstd.h
# End Source File
# Begin Source File

SOURCE=..\src\pconsts.h
# End Source File
# Begin Source File

SOURCE=..\src\prefixes.h
# End Source File
# Begin Source File

SOURCE=.\Solver.h
# End Source File
# Begin Source File

SOURCE=..\src\units.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
