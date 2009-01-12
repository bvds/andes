# Microsoft Developer Studio Generated NMAKE File, Based on Fbd.dsp
!IF "$(CFG)" == ""
CFG=FBD - Win32 DLL Debug
!MESSAGE No configuration specified. Defaulting to FBD - Win32 DLL Debug.
!ENDIF 

!IF "$(CFG)" != "FBD - Win32 Release" && "$(CFG)" != "FBD - Win32 Debug" && "$(CFG)" != "FBD - Win32 Atlas Debug" && "$(CFG)" != "FBD - Win32 Atlas Release" && "$(CFG)" != "FBD - Win32 TCP Debug" && "$(CFG)" != "FBD - Win32 TCP Release" && "$(CFG)" != "FBD - Win32 WOZ Debug" && "$(CFG)" != "FBD - Win32 WOZ Release" && "$(CFG)" != "FBD - Win32 Roving Release" && "$(CFG)" != "FBD - Win32 Roving Debug" && "$(CFG)" != "FBD - Win32 Physics Lite Release" && "$(CFG)" != "FBD - Win32 OLI Release" && "$(CFG)" != "FBD - Win32 OLI Debug" && "$(CFG)" != "FBD - Win32 DLL Debug" && "$(CFG)" != "FBD - Win32 DLL Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
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
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "FBD - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\HelpIfc.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd.exe" 
LINK32_OBJS= \
	"$(INTDIR)\HelpIfc.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\Fbd.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\HelpIfc.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.ilk"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Fbd.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd.exe" 
LINK32_OBJS= \
	"$(INTDIR)\HelpIfc.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

OUTDIR=.\Atlas_Debug
INTDIR=.\Atlas_Debug
# Begin Custom Macros
OutDir=.\Atlas_Debug
# End Custom Macros

ALL : "$(OUTDIR)\Fbd.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\HelpIfc.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.ilk"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Fbd.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd.exe" 
LINK32_OBJS= \
	"$(INTDIR)\HelpIfc.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

OUTDIR=.\Atlas_Release
INTDIR=.\Atlas_Release
# Begin Custom Macros
OutDir=.\Atlas_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\HelpIfc.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd.exe" 
LINK32_OBJS= \
	"$(INTDIR)\HelpIfc.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

OUTDIR=.\TCP_Debug
INTDIR=.\TCP_Debug
# Begin Custom Macros
OutDir=.\TCP_Debug
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-tcp.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-tcp.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-tcp.exe"
	-@erase "$(OUTDIR)\Fbd-tcp.ilk"
	-@erase "$(OUTDIR)\Fbd-tcp.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib wininet.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Fbd-tcp.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-tcp.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-tcp.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-tcp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

OUTDIR=.\TCP_Release
INTDIR=.\TCP_Release
# Begin Custom Macros
OutDir=.\TCP_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-tcp.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-tcp.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-tcp.exe"
	-@erase "$(OUTDIR)\Fbd-tcp.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd-tcp.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-tcp.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-tcp.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-tcp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\TCP_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent directory
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\TCP_Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Fbd-tcp.exe"
   copy .\TCP_Release\fbd-tcp.exe ..
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

OUTDIR=.\WOZ_Debug
INTDIR=.\WOZ_Debug
# Begin Custom Macros
OutDir=.\WOZ_Debug
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-woz.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-tcp.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-woz.exe"
	-@erase "$(OUTDIR)\Fbd-woz.ilk"
	-@erase "$(OUTDIR)\Fbd-woz.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Fbd-woz.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-woz.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-tcp.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-woz.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

OUTDIR=.\WOZ_Release
INTDIR=.\WOZ_Release
# Begin Custom Macros
OutDir=.\WOZ_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-woz.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-tcp.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-woz.exe"
	-@erase "$(OUTDIR)\Fbd-woz.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd-woz.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-woz.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-tcp.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-woz.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

OUTDIR=.\Roving_Release
INTDIR=.\Roving_Release
# Begin Custom Macros
OutDir=.\Roving_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-net.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-tcp.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\TransferDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-net.exe"
	-@erase "$(OUTDIR)\Fbd-net.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib wininet.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd-net.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-net.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-tcp.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\TransferDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-net.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

TargetPath=.\Roving_Release\Fbd-net.exe
SOURCE="$(InputPath)"
PostBuild_Desc=copying $(TargetPath) into C:\Andes2
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Roving_Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Fbd-net.exe"
   copy .\Roving_Release\Fbd-net.exe C:\Andes2
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

OUTDIR=.\Roving_Debug
INTDIR=.\Roving_Debug
# Begin Custom Macros
OutDir=.\Roving_Debug
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-net.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-tcp.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\TransferDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-net.exe"
	-@erase "$(OUTDIR)\Fbd-net.ilk"
	-@erase "$(OUTDIR)\Fbd-net.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib wininet.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Fbd-net.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-net.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-tcp.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\TransferDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-net.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

OUTDIR=.\Lite_Release
INTDIR=.\Lite_Release
# Begin Custom Macros
OutDir=.\Lite_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-lite.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-tcp.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-lite.exe"
	-@erase "$(OUTDIR)\Fbd-lite.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd-lite.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-lite.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-tcp.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-lite.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\Lite_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into C:\Andes2
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Lite_Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Fbd-lite.exe"
   copy .\Lite_Release\fbd-lite.exe C:\Andes2
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

OUTDIR=.\OLI_Release
INTDIR=.\OLI_Release
# Begin Custom Macros
OutDir=.\OLI_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-tcp.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-dll.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-tcp.exe"
	-@erase "$(OUTDIR)\Fbd-tcp.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib ../helpifc.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd-tcp.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-tcp.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-dll.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-tcp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\OLI_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent dir
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\OLI_Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Fbd-tcp.exe"
   copy .\OLI_Release\fbd-tcp.exe ..\fbd-tcp-oli.exe
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

OUTDIR=.\OLI_Debug
INTDIR=.\OLI_Debug
# Begin Custom Macros
OutDir=.\OLI_Debug
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-tcp.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-dll.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-tcp.exe"
	-@erase "$(OUTDIR)\Fbd-tcp.ilk"
	-@erase "$(OUTDIR)\Fbd-tcp.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=wininet.lib winmm.lib version.lib ../helpifc.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Fbd-tcp.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-tcp.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-dll.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-tcp.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

OUTDIR=.\DLL_Debug
INTDIR=.\DLL_Debug
# Begin Custom Macros
OutDir=.\DLL_Debug
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-dll.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-dll.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-dll.exe"
	-@erase "$(OUTDIR)\Fbd-dll.ilk"
	-@erase "$(OUTDIR)\Fbd-dll.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "_DEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib wininet.lib ../helpifc.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\Fbd-dll.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-dll.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-dll.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-dll.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\DLL_Debug
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent directory
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\DLL_Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Fbd-dll.exe"
   copy .\DLL_Debug\fbd-dll.exe ..
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

OUTDIR=.\DLL_Release
INTDIR=.\DLL_Release
# Begin Custom Macros
OutDir=.\DLL_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-dll.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\Base64Coder.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\CurrentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DipoleDlg.obj"
	-@erase "$(INTDIR)\DocManagerEx.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\exhintdg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\expbdydg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\extxtdlg.obj"
	-@erase "$(INTDIR)\exview.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
	-@erase "$(INTDIR)\FieldDlg.obj"
	-@erase "$(INTDIR)\FormulaDlg.obj"
	-@erase "$(INTDIR)\GreekOpts.obj"
	-@erase "$(INTDIR)\GridCtrl.obj"
	-@erase "$(INTDIR)\GridDropTarget.obj"
	-@erase "$(INTDIR)\HelpFilesPage.obj"
	-@erase "$(INTDIR)\helpifc-dll.obj"
	-@erase "$(INTDIR)\HiLevelVw.obj"
	-@erase "$(INTDIR)\HintDlg.obj"
	-@erase "$(INTDIR)\HintView.obj"
	-@erase "$(INTDIR)\history.obj"
	-@erase "$(INTDIR)\HlpOpDlg.obj"
	-@erase "$(INTDIR)\HypertxtDlg.obj"
	-@erase "$(INTDIR)\ImpulseDlg.obj"
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
	-@erase "$(INTDIR)\LineDlg.obj"
	-@erase "$(INTDIR)\LispReader.obj"
	-@erase "$(INTDIR)\listvwex.obj"
	-@erase "$(INTDIR)\LogEdit.obj"
	-@erase "$(INTDIR)\LoginDlg.obj"
	-@erase "$(INTDIR)\MainFrm.obj"
	-@erase "$(INTDIR)\MCQDlg.obj"
	-@erase "$(INTDIR)\MDIFixedSizeFrm.obj"
	-@erase "$(INTDIR)\messages.obj"
	-@erase "$(INTDIR)\MotDlg.obj"
	-@erase "$(INTDIR)\Motion.obj"
	-@erase "$(INTDIR)\MyGrids.obj"
	-@erase "$(INTDIR)\MyMenu.obj"
	-@erase "$(INTDIR)\OliView.obj"
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PictCtrl.obj"
	-@erase "$(INTDIR)\Picture.obj"
	-@erase "$(INTDIR)\planobj.obj"
	-@erase "$(INTDIR)\planstrs.obj"
	-@erase "$(INTDIR)\planview.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProbDlg.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PsmDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\RecordScreen.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RelVelDlg.obj"
	-@erase "$(INTDIR)\ResistanceDlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\StatLink.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TimeConstantDlg.obj"
	-@erase "$(INTDIR)\TorqueDipoleDlg.obj"
	-@erase "$(INTDIR)\TorqueDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\UnitVectorDlg.obj"
	-@erase "$(INTDIR)\ValueDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VecPosDlg.obj"
	-@erase "$(INTDIR)\VideoDlg.obj"
	-@erase "$(INTDIR)\VoltageDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-dll.exe"
	-@erase "$(OUTDIR)\Fbd-dll.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\FBD.res" /d "NDEBUG" /d "_AFXDLL" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\Fbd.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=winmm.lib version.lib wininet.lib ../helpifc.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\Fbd-dll.pdb" /debug /machine:I386 /out:"$(OUTDIR)\Fbd-dll.exe" 
LINK32_OBJS= \
	"$(INTDIR)\helpifc-dll.obj" \
	"$(INTDIR)\history.obj" \
	"$(INTDIR)\AngleDlg.obj" \
	"$(INTDIR)\AreaDlg.obj" \
	"$(INTDIR)\AuthDlg.obj" \
	"$(INTDIR)\AxesDlg.obj" \
	"$(INTDIR)\Base64Coder.obj" \
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\CurrentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DipoleDlg.obj" \
	"$(INTDIR)\DocManagerEx.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DoneDlg.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\exhintdg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\expbdydg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\extxtdlg.obj" \
	"$(INTDIR)\exview.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
	"$(INTDIR)\FieldDlg.obj" \
	"$(INTDIR)\FormulaDlg.obj" \
	"$(INTDIR)\GreekOpts.obj" \
	"$(INTDIR)\GridCtrl.obj" \
	"$(INTDIR)\GridDropTarget.obj" \
	"$(INTDIR)\HelpFilesPage.obj" \
	"$(INTDIR)\HiLevelVw.obj" \
	"$(INTDIR)\HintDlg.obj" \
	"$(INTDIR)\HintView.obj" \
	"$(INTDIR)\HlpOpDlg.obj" \
	"$(INTDIR)\HypertxtDlg.obj" \
	"$(INTDIR)\ImpulseDlg.obj" \
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
	"$(INTDIR)\LineDlg.obj" \
	"$(INTDIR)\LispReader.obj" \
	"$(INTDIR)\listvwex.obj" \
	"$(INTDIR)\LogEdit.obj" \
	"$(INTDIR)\LoginDlg.obj" \
	"$(INTDIR)\MainFrm.obj" \
	"$(INTDIR)\MCQDlg.obj" \
	"$(INTDIR)\MDIFixedSizeFrm.obj" \
	"$(INTDIR)\messages.obj" \
	"$(INTDIR)\MotDlg.obj" \
	"$(INTDIR)\Motion.obj" \
	"$(INTDIR)\MyGrids.obj" \
	"$(INTDIR)\MyMenu.obj" \
	"$(INTDIR)\OliView.obj" \
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\PictCtrl.obj" \
	"$(INTDIR)\Picture.obj" \
	"$(INTDIR)\planobj.obj" \
	"$(INTDIR)\planstrs.obj" \
	"$(INTDIR)\planview.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProbDlg.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PsmDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\RecordScreen.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RelVelDlg.obj" \
	"$(INTDIR)\ResistanceDlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\StatLink.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\TimeConstantDlg.obj" \
	"$(INTDIR)\TorqueDipoleDlg.obj" \
	"$(INTDIR)\TorqueDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\UnitVectorDlg.obj" \
	"$(INTDIR)\ValueDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VecPosDlg.obj" \
	"$(INTDIR)\VideoDlg.obj" \
	"$(INTDIR)\VoltageDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res"

"$(OUTDIR)\Fbd-dll.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\DLL_Release
SOURCE="$(InputPath)"
PostBuild_Desc=copying into parent directory
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\DLL_Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\Fbd-dll.exe"
   copy .\DLL_Release\fbd-dll.exe ..
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("Fbd.dep")
!INCLUDE "Fbd.dep"
!ELSE 
!MESSAGE Warning: cannot find "Fbd.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "FBD - Win32 Release" || "$(CFG)" == "FBD - Win32 Debug" || "$(CFG)" == "FBD - Win32 Atlas Debug" || "$(CFG)" == "FBD - Win32 Atlas Release" || "$(CFG)" == "FBD - Win32 TCP Debug" || "$(CFG)" == "FBD - Win32 TCP Release" || "$(CFG)" == "FBD - Win32 WOZ Debug" || "$(CFG)" == "FBD - Win32 WOZ Release" || "$(CFG)" == "FBD - Win32 Roving Release" || "$(CFG)" == "FBD - Win32 Roving Debug" || "$(CFG)" == "FBD - Win32 Physics Lite Release" || "$(CFG)" == "FBD - Win32 OLI Release" || "$(CFG)" == "FBD - Win32 OLI Debug" || "$(CFG)" == "FBD - Win32 DLL Debug" || "$(CFG)" == "FBD - Win32 DLL Release"
SOURCE=".\helpifc-dll.cpp"

!IF  "$(CFG)" == "FBD - Win32 Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"


"$(INTDIR)\helpifc-dll.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"


"$(INTDIR)\helpifc-dll.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"


"$(INTDIR)\helpifc-dll.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"


"$(INTDIR)\helpifc-dll.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ENDIF 

SOURCE=".\helpifc-tcp.cpp"

!IF  "$(CFG)" == "FBD - Win32 Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

!ENDIF 

SOURCE=.\HelpIfc.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"


"$(INTDIR)\HelpIfc.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"


"$(INTDIR)\HelpIfc.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"


"$(INTDIR)\HelpIfc.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"


"$(INTDIR)\HelpIfc.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

!ENDIF 

SOURCE=.\history.cpp

"$(INTDIR)\history.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\AngleDlg.cpp

"$(INTDIR)\AngleDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\AreaDlg.cpp

"$(INTDIR)\AreaDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\AuthDlg.cpp

"$(INTDIR)\AuthDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\AxesDlg.cpp

"$(INTDIR)\AxesDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Base64Coder.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Base64Coder.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\BrowsDlg.cpp

"$(INTDIR)\BrowsDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ChatView.cpp

"$(INTDIR)\ChatView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\CheckedDlg.cpp

"$(INTDIR)\CheckedDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Childfrm.cpp

"$(INTDIR)\Childfrm.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ChoiceDlg.cpp

"$(INTDIR)\ChoiceDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\CommentDlg.cpp

"$(INTDIR)\CommentDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\CurrentDlg.cpp

"$(INTDIR)\CurrentDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\DemoDlg.cpp

"$(INTDIR)\DemoDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\DipoleDlg.cpp

"$(INTDIR)\DipoleDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\DocManagerEx.cpp

"$(INTDIR)\DocManagerEx.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\DocPages.cpp

"$(INTDIR)\DocPages.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Example\DoneDlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\DrawObj.cpp

"$(INTDIR)\DrawObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\DrawObjDlg.cpp

"$(INTDIR)\DrawObjDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\EnergyDlg.cpp

"$(INTDIR)\EnergyDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\EQEdit.cpp

"$(INTDIR)\EQEdit.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\EQView.cpp

"$(INTDIR)\EQView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Example\exhintdg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exhintdg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\Exp2Dlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\Exp4Dlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\expbdydg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\expbdydg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\EXPlanVw.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\ExpLawDlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\expmenu.rc
SOURCE=.\Example\extxtdlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\extxtdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\exview.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\exview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\FBD.cpp

"$(INTDIR)\FBD.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\FBDDoc.cpp

"$(INTDIR)\FBDDoc.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\FBDObj.cpp

"$(INTDIR)\FBDObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\fbdview.cpp

"$(INTDIR)\fbdview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\FieldDlg.cpp

"$(INTDIR)\FieldDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\FormulaDlg.cpp

"$(INTDIR)\FormulaDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\GreekOpts.cpp

"$(INTDIR)\GreekOpts.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\GridCtrl.cpp

"$(INTDIR)\GridCtrl.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\GridDropTarget.cpp

"$(INTDIR)\GridDropTarget.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\HelpFilesPage.cpp

"$(INTDIR)\HelpFilesPage.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\HiLevelVw.cpp

"$(INTDIR)\HiLevelVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\HintDlg.cpp

"$(INTDIR)\HintDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\HintView.cpp

"$(INTDIR)\HintView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\HlpOpDlg.cpp

"$(INTDIR)\HlpOpDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\HypertxtDlg.cpp

"$(INTDIR)\HypertxtDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ImpulseDlg.cpp

"$(INTDIR)\ImpulseDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\InPlaceList.cpp

"$(INTDIR)\InPlaceList.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ItemCtrl.cpp

"$(INTDIR)\ItemCtrl.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\LabelDlg.cpp

"$(INTDIR)\LabelDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\LabRadDlg.cpp

"$(INTDIR)\LabRadDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\LawDialog.cpp

"$(INTDIR)\LawDialog.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\LineDlg.cpp

"$(INTDIR)\LineDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\LispReader.cpp

"$(INTDIR)\LispReader.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\listvwex.cpp

"$(INTDIR)\listvwex.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\LogEdit.cpp

"$(INTDIR)\LogEdit.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\LoginDlg.cpp

"$(INTDIR)\LoginDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\MainFrm.cpp

"$(INTDIR)\MainFrm.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\MCQDlg.cpp

"$(INTDIR)\MCQDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\MDIFixedSizeFrm.cpp

"$(INTDIR)\MDIFixedSizeFrm.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\messages.cpp

"$(INTDIR)\messages.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\MotDlg.cpp

"$(INTDIR)\MotDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Motion.cpp

"$(INTDIR)\Motion.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\MyGrids.cpp

"$(INTDIR)\MyGrids.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\MyMenu.cpp

"$(INTDIR)\MyMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\OliView.cpp

"$(INTDIR)\OliView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PicCtrl.cpp

"$(INTDIR)\PicCtrl.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PictCtrl.cpp

"$(INTDIR)\PictCtrl.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Picture.cpp

"$(INTDIR)\Picture.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Example\planobj.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planobj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\planstrs.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planstrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\planview.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\planview.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Playdlg.cpp

"$(INTDIR)\Playdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PopupWnd.cpp

"$(INTDIR)\PopupWnd.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PrincView.cpp

"$(INTDIR)\PrincView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ProbDlg.cpp

"$(INTDIR)\ProbDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ProblemSet.cpp

"$(INTDIR)\ProblemSet.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ProbSetEditView.cpp

"$(INTDIR)\ProbSetEditView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ProbSetView.cpp

"$(INTDIR)\ProbSetView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PropertyDlg.cpp

"$(INTDIR)\PropertyDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PsmDlg.cpp

"$(INTDIR)\PsmDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PtrDlg.cpp

"$(INTDIR)\PtrDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\RecordScreen.cpp

"$(INTDIR)\RecordScreen.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Rectdlg.cpp

"$(INTDIR)\Rectdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\RelVelDlg.cpp

"$(INTDIR)\RelVelDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ResistanceDlg.cpp

"$(INTDIR)\ResistanceDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\RichCombo.cpp

"$(INTDIR)\RichCombo.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\RichEditEx.cpp

"$(INTDIR)\RichEditEx.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\RuleQDlg.cpp

"$(INTDIR)\RuleQDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\SolveVarDlg.cpp

"$(INTDIR)\SolveVarDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Splash.cpp

"$(INTDIR)\Splash.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\StageObj.cpp

"$(INTDIR)\StageObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\StatLink.cpp

"$(INTDIR)\StatLink.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Stdafx.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\stepdlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\SymbolMenu.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\SysDlg.cpp

"$(INTDIR)\SysDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\TabView.cpp

"$(INTDIR)\TabView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\TaskDlg.cpp

"$(INTDIR)\TaskDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Example\TemplateDlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "ROVING" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "LITE" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /D "OLI" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /Zi /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\TimeConstantDlg.cpp

"$(INTDIR)\TimeConstantDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\TorqueDipoleDlg.cpp

"$(INTDIR)\TorqueDipoleDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\TorqueDlg.cpp

"$(INTDIR)\TorqueDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\TraceDlg.cpp

"$(INTDIR)\TraceDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\TransferDlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 WOZ Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Release"


"$(INTDIR)\TransferDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Roving Debug"


"$(INTDIR)\TransferDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 Physics Lite Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 OLI Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 DLL Release"

!ENDIF 

SOURCE=.\UnitVectorDlg.cpp

"$(INTDIR)\UnitVectorDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ValueDlg.cpp

"$(INTDIR)\ValueDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VariableDlg.cpp

"$(INTDIR)\VariableDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VarView.cpp

"$(INTDIR)\VarView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VecAVDlg.cpp

"$(INTDIR)\VecAVDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VecCpDlg.cpp

"$(INTDIR)\VecCpDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VecDlg.cpp

"$(INTDIR)\VecDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VecPosDlg.cpp

"$(INTDIR)\VecPosDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VideoDlg.cpp

"$(INTDIR)\VideoDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VoltageDlg.cpp

"$(INTDIR)\VoltageDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\VwOptDlg.cpp

"$(INTDIR)\VwOptDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\wbrowser.cpp

"$(INTDIR)\wbrowser.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\FBD.rc

"$(INTDIR)\FBD.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

