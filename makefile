# 
# Makefile for building Andes system components from sources
#
# Assumes MSDEV 98 for C++ components and Allegro Common Lisp 8.1 for
# help system runtime images. Adjust commands for different tools

# where to run Allegro Common Lisp
ACL = C:/Progra~1/acl81/alisp.exe 

# MS Developer Studio. This might be in path if installed properly
MSDEV = C:/Program\ Files/Microsoft\ Visual\ Studio/Common/MSDev98/Bin/msdev

# following used to compile helpifc.dll, which is not an msdev project
win_lib = user32.lib gdi32.lib kernel32.lib comctl32.lib comdlg32.lib \
	winmm.lib  msvcrt.lib
CC = cl  -nologo -Od -c -Zi -I -W3 -G3 
link = C:/Program\ Files/Microsoft\ Visual\ Studio/VC98/Bin/link -nologo

# all makes all our executables
all: helpsys workbench solver installer helpifc.dll

# two versions of help system image. TCP version now only used when developing,
# so binary image maybe not needed.
helpsys: helpsys-dll helpsys-tcp

helpsys-dll: 	# always rebuild, dependencies too complicated
	$(ACL) -L makedll.cl 

helpsys-tcp:    # always rebuild, dependencies too complicated
	$(ACL) -L makehelp80.cl

# three versions of workbench: oli, regular, plus tcp version for development
workbench: 
	$(MSDEV) Fbd/fbd.dsp /MAKE "FBD - Win32 DLL Release"
	$(MSDEV) Fbd/fbd.dsp /MAKE "FBD - Win32 OLI Release"
	$(MSDEV) Fbd/fbd.dsp /MAKE "FBD - Win32 TCP Release"

# setup program, OLI and standalone, plus uninstaller 
installer: 
	$(MSDEV) Setup/Setup.dsp /MAKE "Setup - Win32 Student Release"
	$(MSDEV) Setup/Setup.dsp /MAKE "Setup - Win32 OLI Release"
	$(MSDEV) Uninst/Uninst.dsp /MAKE "Uninst - Win32 Release"

# solver dll.
solver:
	$(MSDEV) Algebra/src/solver.dsp /MAKE "Solver - Win32 Debug"

	
# helpifc.dll -- adapted from factorial sample in Allegro installation
helpifc.dll:	helpifc.obj lnkacl.lib
	$(link) -dll  -debug -out:helpifc.dll helpifc.obj $(win_lib) \
		lnkacl.lib

helpifc.obj:	helpifc.c
	$(CC) helpifc.c -Fohelpifc.obj

FORCE:
