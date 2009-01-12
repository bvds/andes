@echo off
REM -- First make map file from Microsoft Visual C++ generated resource.h
echo // MAKEHELP.BAT generated Help Map file.  Used by FBD.HPJ. >"hlp\FBD.hm"
echo. >>"hlp\FBD.hm"
echo // Commands (ID_* and IDM_*) >>"hlp\FBD.hm"
makehm ID_,HID_,0x10000 IDM_,HIDM_,0x10000 resource.h >>"hlp\FBD.hm"
echo. >>"hlp\FBD.hm"
echo // Prompts (IDP_*) >>"hlp\FBD.hm"
makehm IDP_,HIDP_,0x30000 resource.h >>"hlp\FBD.hm"
echo. >>"hlp\FBD.hm"
echo // Resources (IDR_*) >>"hlp\FBD.hm"
makehm IDR_,HIDR_,0x20000 resource.h >>"hlp\FBD.hm"
echo. >>"hlp\FBD.hm"
echo // Dialogs (IDD_*) >>"hlp\FBD.hm"
makehm IDD_,HIDD_,0x20000 resource.h >>"hlp\FBD.hm"
echo. >>"hlp\FBD.hm"
echo // Frame Controls (IDW_*) >>"hlp\FBD.hm"
makehm IDW_,HIDW_,0x50000 resource.h >>"hlp\FBD.hm"
REM -- Make help for Project FBD


echo Building Win32 Help files
start /wait hcrtf -x "hlp\FBD.hpj"
echo.
if exist Debug\nul copy "hlp\FBD.hlp" Debug
if exist Debug\nul copy "hlp\FBD.cnt" Debug
if exist Release\nul copy "hlp\FBD.hlp" Release
if exist Release\nul copy "hlp\FBD.cnt" Release
echo.


