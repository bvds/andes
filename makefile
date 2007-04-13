# Makefile to compile helpifc.dll
# Copied from factorial sample in Allegro installation

win_lib = user32.lib gdi32.lib kernel32.lib comctl32.lib comdlg32.lib \
	winmm.lib  msvcrt.lib

win_lib = 

CC = cl  -nologo -Od -c -Zi -I -W3 -G3 

link = C:/Program\ Files/Microsoft\ Visual\ Studio/VC98/Bin/link -nologo

RUNLISP = sh ../runlisp.sh

xcl = ../lispi -I ../dcli.dxl

oldspace = 512000

helpifc.dll:	helpifc.obj lnkacl.lib
	$(link) -dll  -debug -out:helpifc.dll helpifc.obj $(win_lib) \
		lnkacl.lib

helpifc.obj:	helpifc.c
	$(CC) helpifc.c -Fohelpifc.obj

#ftest.exe:	ftest.obj fact.dll lnkacl.dll
#	$(link) -debug -out:ftest.exe ftest.obj fact.lib
#
#ftest.obj:	ftest.c
#	$(CC) ftest.c -Foftest.obj

# assumes lisp built already in ..

helpsys.dxl:	helpifc.cl lnk.cl 
	rm -fr fact
	(echo '(generate-application "fact" "fact/" (list (load-compiled "lnk.cl") \
	(load-compiled "fact.cl") :process) :application-type :exe :application-files \
	(list "../lnkacl.dll" "helpifc.dll") :restart-init-function nil \
	:restart-app-function nil :include-compiler nil \
	:include-ide nil)';\
	 	echo '(exit)') > buildf.tmp
	$(RUNLISP) -t helpsys.dxl -f buildf.tmp $(xcl)
	$(AT)rm buildf.tmp

# assumes lnkacl.dll made in /cl/src directory

clean: FORCE
	rm -fr fact
	rm -f *.dll *.exe *.fasl *.out *.lib *.obj *.pdb *.ilk *.exp

FORCE:
