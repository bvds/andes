# 
# Makefile for building Andes system components from sources
#
# Used Visual C++ 6.0 from Visual Studio 6.0 (1998) for C++ components 
# Allegro Common Lisp 8.1 for help system runtime images. 
# Adjust commands for different tools

# where to run Allegro Common Lisp
ACL = C:/Progra~1/acl81/alisp.exe 

# MS Developer Studio for building C++ projects. 
# This should be in the Windows path if VC++ was setup for command line builds
# via vcvars32.bat
# MSDEV = msdev
# cygwin will import these from Windows PATH if installed after VC++; 
# otherwise, must get these into cygwin path.
# If not, use absolute path like one below
MSDEV = C:/Program\ Files/Microsoft\ Visual\ Studio/Common/MSDev98/Bin/msdev

# following used to compile and link helpifc.dll, which is not an msdev project
# use full path on link command since cygwin link can shadow it
win_lib = user32.lib gdi32.lib kernel32.lib comctl32.lib comdlg32.lib \
	winmm.lib  msvcrt.lib
CC = cl  -nologo -Od -c -Zi -I -W3 -G3 
link = C:/Program\ Files/Microsoft\ Visual\ Studio/VC98/Bin/link -nologo

# all makes all our executables
all: helpsys workbench solver installer helpifc.dll

# two versions of help system image. TCP version now only used when developing,
# so binary image maybe not needed.
helpsys: helpsys-dll helpsys-tcp

#  lnk.cl needed by Franz lisp, can be copied from distribution
helpsys-dll: 	# always rebuild, dependencies too complicated
	cp -u -p /cygdrive/c/Program\ Files/acl81/examples/dll/lnk.cl .
	$(ACL) -L makedll.cl 

helpsys-tcp:    # always rebuild, dependencies too complicated
	$(ACL) -L makehelp80.cl

# three versions of workbench: oli, standalone (both using DLL helpsys) 
# plus standalone tcp version for development
workbench: 
	$(MSDEV) Fbd/fbd.dsp /MAKE "FBD - Win32 DLL Release"
	$(MSDEV) Fbd/fbd.dsp /MAKE "FBD - Win32 OLI Release"
	$(MSDEV) Fbd/fbd.dsp /MAKE "FBD - Win32 TCP Release"
	mv -v FBD/*_Release/Fbd-*.exe .

# setup program: OLI and standalone, versions plus uninstaller 
installer: 
	$(MSDEV) Setup/Setup.dsp /MAKE "Setup - Win32 Student Release"
	$(MSDEV) Setup/Setup.dsp /MAKE "Setup - Win32 OLI Release"
	$(MSDEV) Uninst/Uninst.dsp /MAKE "Uninst - Win32 Release"
	mv -v Setup/*_Release/Setup*.exe .
	mv -v Uninst/Release/Uninst.exe .

# solver dll.
solver:
	$(MSDEV) Algebra/src/solver.dsp /MAKE "Solver - Win32 Debug"
	mv -v Algebra/src/Debug/Solver.dll .

# helpifc.dll -- adapted from factorial sample in Allegro installation
helpifc.dll:	helpifc.obj lnkacl.lib
	$(link) -dll  -debug -out:helpifc.dll helpifc.obj $(win_lib) \
		lnkacl.lib

helpifc.obj:	helpifc.c
	$(CC) helpifc.c -Fohelpifc.obj

# Problem statements
Statements:    #always do
	mkdir -p Statements
	$(ACL) -L statements.cl

FORCE:
