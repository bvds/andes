@set buildroot=C:\Andes2
@set buildfbd=C:\Andes2\fbd
@set buildsetup=C:\Andes2\setup
@set builduninst=C:\Andes2\uninst
@set buildupload=C:\Andes2\upload
@set buildss="C:\Program Files\Microsoft Visual Studio\VSS\win32\ss"
@set buildmsdev="C:\Program Files\Microsoft Visual Studio\Common\MSDev98\Bin\msdev"
@set buildlisp="C:\Program Files\ACL501\lisp.exe"
@set buildwinsys=C:\Windows\System
@set buildvla2=T:\VanLehn\Andes2
@set buildwz="C:\Program Files\WinZip\wzzip"
@set buildwzse="C:\Program Files\WinZip\Winzip Self-Extractor\WinZipSE"

@set ssdir=\\explore\home\vanlehn\Andes\Shared\VSS

@if !%2==!ALL goto compile
@if !%2==!ALLC goto compile
@if !%2==!UPDATE goto zip
@if !%2==!UPDATEC goto zip
@echo USAGE: build DestinationDir [UPDATE UPDATEC ALL ALLC] [STEP]
@echo ..DestinationDir is directory that will built for creating distribution.
@echo ..ALL(C) action builds complete distributable beginning with initial get
@echo .... with source safe through build of distributable self-extracting archive.
@echo ..UPDATE(C) rebuilds zip files only (for new problems and review items.)
@echo ..(C) builds with lisp console active
@echo ..STEP ... allows single stepping of major steps {control-c will abort}.
@echo .... Argument options UPDATE and STEP are case-sensitive.
@echo ..
@echo ..WARNING: edit the build variables at front of this file before executing
@echo ..NOTE: first time you execute this there may be a significant delay
@echo ....while SourceSafe gets files the first time.
@echo ..NOTE: remember to update versions and other files before executing build.
@goto done

:compile

@cd %buildfbd%
@%buildss% get $/fbd
@if !%3==!STEP pause
@%buildmsdev% fbd.dsp /Make "FBD - Win32 TCP Release" /REBUILD
@if !%3==!STEP pause

@cd %buildroot%
@%buildss% get $/Andes2
@if !%3==!STEP pause

@cd %buildroot%
@del Base\*.fasl
@del Help\*.fasl
@del HelpStructs\*.fasl
@del Knowledge\*.fasl
@cd %buildroot%\Help
@%buildlisp% -L makedist.cl -kill
:wait4dxl
@if not exist %buildroot%\Andes2.dxl goto wait4dxl
@if !%3==!STEP pause

@cd %buildroot%\Algebra\Solver
@del Debug\*.obj
@del Debug\*.pch
@del Debug\vc60.*
@%buildmsdev% Solver.dsp /MAKE "Solver - Win32 Debug" /REBUILD
@if !%3==!STEP pause

@cd %buildsetup%
@%buildss" get $/Setup
@if !%3==!STEP pause
@%buildmsdev% Setup.dsp /MAKE "Setup - Win32 USNA Eval Release" /REBUILD
@if !%3==!STEP pause

@cd %builduninst%
@%buildss% get $/Uninst
@if !%3==!STEP pause
@%buildmsdev% Uninst.dsp /MAKE "Uninst - Win32 Release" /REBUILD
@if !%3==!STEP pause

@cd %buildupload%
@%buildss% get $/upload
@if !%3==!STEP pause
@%buildmsdev% upload.dsp /MAKE "upload - Win32 Release" /REBUILD
@if !%3==!STEP pause

@cd %buildroot%

:zip
@copy %buildfbd%\hlp\fbd.cnt %buildroot%
@copy %buildfbd%\hlp\fbd-tcp.hlp %buildroot%
@copy %buildfbd%\TCP_Release\fbd-tcp.exe %buildroot%
@copy %buildwinsys%\acl5016.dll %buildroot%
@if %2!==ALL! copy %buildvla2%\Andes2noconsole.exe %buildroot%\Andes2.exe
@if %2!==UPDATE! copy %buildvla2%\Andes2noconsole.exe %buildroot%\Andes2.exe
@if %2!==ALLC! copy %buildval2%\Andes2console.exe %buildroot%\Andes2.exe
@if %2!==UPDATEC! copy %buildval2%\Andes2console.exe %buildroot%\Andes2.exe

@if not exist %buildroot%\Algebra\Solver\Debug\Solver.dll pause
@Copy %buildroot%\Algebra\Solver\Debug\Solver.dll %buildroot%
@deltree /Y %1
@md %1
@Copy %buildsetup%\USNA_Eval_Release\SetupUSNA.exe %buildroot%
@Copy %builduninst%\Release\uninst.exe %buildroot%
@Copy %buildupload%\Release\upload.exe %buildroot%
@Copy %buildsetup%\USNA_Eval_Release\SetupUSNA.exe %1
@Copy %buildwinsys%\mfc42.dll %1
@Copy %buildwinsys%\msvcrt.dll %1
@cd %buildroot%
@SetupUSNA.exe /copy
@cd %1
@%buildwz% -rp Andes2 *
:wait4zip
@if not exist %1\Andes2.zip goto wait4zip
@Copy %buildroot%\AndesSE.inp %1
@Copy %buildroot%\AndesSE0.txt %1
@Copy %buildroot%\AndesSE1.txt %1
@attrib -R *

@%buildwzse% Andes2 @AndesSE.inp
@cd %buildroot%
:done
