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

@if not !%1==! goto zip
@echo Usage: awbuild DestinationDir (C)
@goto done

:zip
@copy %buildfbd%\hlp\fbd.cnt %buildroot%
@copy %buildfbd%\hlp\fbd-tcp.hlp %buildroot%
@copy %buildfbd%\TCP_Release\fbd-tcp.exe %buildroot%
@copy %buildwinsys%\acl5016.dll %buildroot%
@if %2!==! copy %buildvla2%\Andes2noconsole.exe %buildroot%\Andes2.exe
@if %2!==C! copy %buildval2%\Andes2console.exe %buildroot%\Andes2.exe

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
