# Microsoft Developer Studio Generated NMAKE File, Based on Fbd.dsp
!IF "$(CFG)" == ""
CFG=FBD - Win32 TCP Debug
!MESSAGE No configuration specified. Defaulting to FBD - Win32 TCP Debug.
!ENDIF 

!IF "$(CFG)" != "FBD - Win32 Release" && "$(CFG)" != "FBD - Win32 Debug" && "$(CFG)" != "FBD - Win32 Atlas Debug" && "$(CFG)" != "FBD - Win32 Atlas Release" && "$(CFG)" != "FBD - Win32 TCP Debug" && "$(CFG)" != "FBD - Win32 TCP Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Fbd.mak" CFG="FBD - Win32 TCP Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "FBD - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Atlas Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 Atlas Release" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 TCP Debug" (based on "Win32 (x86) Application")
!MESSAGE "FBD - Win32 TCP Release" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

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
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\EXHintDg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\ExpBdyDg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\EXTxtDlg.obj"
	-@erase "$(INTDIR)\EXView.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
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
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
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
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PlanObj.obj"
	-@erase "$(INTDIR)\PlanStrs.obj"
	-@erase "$(INTDIR)\PlanView.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
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
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
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
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
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
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res" \
	"$(INTDIR)\EXHintDg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\ExpBdyDg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\EXTxtDlg.obj" \
	"$(INTDIR)\EXView.obj" \
	"$(INTDIR)\PlanObj.obj" \
	"$(INTDIR)\PlanStrs.obj" \
	"$(INTDIR)\PlanView.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\DoneDlg.obj"

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
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\donedlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\EXHintDg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\ExpBdyDg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\EXTxtDlg.obj"
	-@erase "$(INTDIR)\EXView.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
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
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
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
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PlanObj.obj"
	-@erase "$(INTDIR)\PlanStrs.obj"
	-@erase "$(INTDIR)\PlanView.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.ilk"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
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
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
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
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
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
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res" \
	"$(INTDIR)\EXHintDg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\ExpBdyDg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\EXTxtDlg.obj" \
	"$(INTDIR)\EXView.obj" \
	"$(INTDIR)\PlanObj.obj" \
	"$(INTDIR)\PlanStrs.obj" \
	"$(INTDIR)\PlanView.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\donedlg.obj"

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
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\EXHintDg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\ExpBdyDg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\EXTxtDlg.obj"
	-@erase "$(INTDIR)\EXView.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
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
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
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
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PlanObj.obj"
	-@erase "$(INTDIR)\PlanStrs.obj"
	-@erase "$(INTDIR)\PlanView.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.ilk"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
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
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
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
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
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
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res" \
	"$(INTDIR)\EXHintDg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\ExpBdyDg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\EXTxtDlg.obj" \
	"$(INTDIR)\EXView.obj" \
	"$(INTDIR)\PlanObj.obj" \
	"$(INTDIR)\PlanStrs.obj" \
	"$(INTDIR)\PlanView.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\DoneDlg.obj"

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
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\EXHintDg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\ExpBdyDg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\EXTxtDlg.obj"
	-@erase "$(INTDIR)\EXView.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
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
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
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
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PlanObj.obj"
	-@erase "$(INTDIR)\PlanStrs.obj"
	-@erase "$(INTDIR)\PlanView.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd.exe"
	-@erase "$(OUTDIR)\Fbd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
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
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
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
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
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
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res" \
	"$(INTDIR)\EXHintDg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\ExpBdyDg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\EXTxtDlg.obj" \
	"$(INTDIR)\EXView.obj" \
	"$(INTDIR)\PlanObj.obj" \
	"$(INTDIR)\PlanStrs.obj" \
	"$(INTDIR)\PlanView.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\DoneDlg.obj"

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

ALL : "$(OUTDIR)\Fbd-woz.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\EXHintDg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\ExpBdyDg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\EXTxtDlg.obj"
	-@erase "$(INTDIR)\EXView.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
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
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
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
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PlanObj.obj"
	-@erase "$(INTDIR)\PlanStrs.obj"
	-@erase "$(INTDIR)\PlanView.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-woz.exe"
	-@erase "$(OUTDIR)\Fbd-woz.ilk"
	-@erase "$(OUTDIR)\Fbd-woz.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
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
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
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
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
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
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res" \
	"$(INTDIR)\EXHintDg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\ExpBdyDg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\EXTxtDlg.obj" \
	"$(INTDIR)\EXView.obj" \
	"$(INTDIR)\PlanObj.obj" \
	"$(INTDIR)\PlanStrs.obj" \
	"$(INTDIR)\PlanView.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\DoneDlg.obj"

"$(OUTDIR)\Fbd-woz.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

OUTDIR=.\TCP_Release
INTDIR=.\TCP_Release
# Begin Custom Macros
OutDir=.\TCP_Release
# End Custom Macros

ALL : "$(OUTDIR)\Fbd-woz.exe"


CLEAN :
	-@erase "$(INTDIR)\AngleDlg.obj"
	-@erase "$(INTDIR)\AreaDlg.obj"
	-@erase "$(INTDIR)\AuthDlg.obj"
	-@erase "$(INTDIR)\AxesDlg.obj"
	-@erase "$(INTDIR)\BrowsDlg.obj"
	-@erase "$(INTDIR)\ChatView.obj"
	-@erase "$(INTDIR)\CheckedDlg.obj"
	-@erase "$(INTDIR)\Childfrm.obj"
	-@erase "$(INTDIR)\ChoiceDlg.obj"
	-@erase "$(INTDIR)\CommentDlg.obj"
	-@erase "$(INTDIR)\DemoDlg.obj"
	-@erase "$(INTDIR)\DocPages.obj"
	-@erase "$(INTDIR)\DoneDlg.obj"
	-@erase "$(INTDIR)\DrawObj.obj"
	-@erase "$(INTDIR)\DrawObjDlg.obj"
	-@erase "$(INTDIR)\EnergyDlg.obj"
	-@erase "$(INTDIR)\EQEdit.obj"
	-@erase "$(INTDIR)\EQView.obj"
	-@erase "$(INTDIR)\EXHintDg.obj"
	-@erase "$(INTDIR)\Exp2Dlg.obj"
	-@erase "$(INTDIR)\Exp4Dlg.obj"
	-@erase "$(INTDIR)\ExpBdyDg.obj"
	-@erase "$(INTDIR)\EXPlanVw.obj"
	-@erase "$(INTDIR)\ExpLawDlg.obj"
	-@erase "$(INTDIR)\EXTxtDlg.obj"
	-@erase "$(INTDIR)\EXView.obj"
	-@erase "$(INTDIR)\FBD.obj"
	-@erase "$(INTDIR)\Fbd.pch"
	-@erase "$(INTDIR)\FBD.res"
	-@erase "$(INTDIR)\FBDDoc.obj"
	-@erase "$(INTDIR)\FBDObj.obj"
	-@erase "$(INTDIR)\fbdview.obj"
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
	-@erase "$(INTDIR)\InPlaceList.obj"
	-@erase "$(INTDIR)\ItemCtrl.obj"
	-@erase "$(INTDIR)\LabelDlg.obj"
	-@erase "$(INTDIR)\LabRadDlg.obj"
	-@erase "$(INTDIR)\LawDialog.obj"
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
	-@erase "$(INTDIR)\PicCtrl.obj"
	-@erase "$(INTDIR)\PlanObj.obj"
	-@erase "$(INTDIR)\PlanStrs.obj"
	-@erase "$(INTDIR)\PlanView.obj"
	-@erase "$(INTDIR)\Playdlg.obj"
	-@erase "$(INTDIR)\PopupWnd.obj"
	-@erase "$(INTDIR)\PrincView.obj"
	-@erase "$(INTDIR)\ProblemSet.obj"
	-@erase "$(INTDIR)\ProbSetEditView.obj"
	-@erase "$(INTDIR)\ProbSetView.obj"
	-@erase "$(INTDIR)\PropertyDlg.obj"
	-@erase "$(INTDIR)\PtrDlg.obj"
	-@erase "$(INTDIR)\Rectdlg.obj"
	-@erase "$(INTDIR)\RichCombo.obj"
	-@erase "$(INTDIR)\RichEditEx.obj"
	-@erase "$(INTDIR)\RuleQDlg.obj"
	-@erase "$(INTDIR)\SolveVarDlg.obj"
	-@erase "$(INTDIR)\Splash.obj"
	-@erase "$(INTDIR)\StageObj.obj"
	-@erase "$(INTDIR)\Stdafx.obj"
	-@erase "$(INTDIR)\stepdlg.obj"
	-@erase "$(INTDIR)\SymbolMenu.obj"
	-@erase "$(INTDIR)\SysDlg.obj"
	-@erase "$(INTDIR)\TabView.obj"
	-@erase "$(INTDIR)\TaskDlg.obj"
	-@erase "$(INTDIR)\TemplateDlg.obj"
	-@erase "$(INTDIR)\TraceDlg.obj"
	-@erase "$(INTDIR)\VariableDlg.obj"
	-@erase "$(INTDIR)\VarView.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\VecAVDlg.obj"
	-@erase "$(INTDIR)\VecCpDlg.obj"
	-@erase "$(INTDIR)\VecDlg.obj"
	-@erase "$(INTDIR)\VwOptDlg.obj"
	-@erase "$(INTDIR)\wbrowser.obj"
	-@erase "$(OUTDIR)\Fbd-woz.exe"
	-@erase "$(OUTDIR)\Fbd-woz.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
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
	"$(INTDIR)\BrowsDlg.obj" \
	"$(INTDIR)\ChatView.obj" \
	"$(INTDIR)\CheckedDlg.obj" \
	"$(INTDIR)\Childfrm.obj" \
	"$(INTDIR)\ChoiceDlg.obj" \
	"$(INTDIR)\CommentDlg.obj" \
	"$(INTDIR)\DemoDlg.obj" \
	"$(INTDIR)\DocPages.obj" \
	"$(INTDIR)\DrawObj.obj" \
	"$(INTDIR)\DrawObjDlg.obj" \
	"$(INTDIR)\EnergyDlg.obj" \
	"$(INTDIR)\EQEdit.obj" \
	"$(INTDIR)\EQView.obj" \
	"$(INTDIR)\FBD.obj" \
	"$(INTDIR)\FBDDoc.obj" \
	"$(INTDIR)\FBDObj.obj" \
	"$(INTDIR)\fbdview.obj" \
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
	"$(INTDIR)\InPlaceList.obj" \
	"$(INTDIR)\ItemCtrl.obj" \
	"$(INTDIR)\LabelDlg.obj" \
	"$(INTDIR)\LabRadDlg.obj" \
	"$(INTDIR)\LawDialog.obj" \
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
	"$(INTDIR)\PicCtrl.obj" \
	"$(INTDIR)\Playdlg.obj" \
	"$(INTDIR)\PopupWnd.obj" \
	"$(INTDIR)\PrincView.obj" \
	"$(INTDIR)\ProblemSet.obj" \
	"$(INTDIR)\ProbSetEditView.obj" \
	"$(INTDIR)\ProbSetView.obj" \
	"$(INTDIR)\PropertyDlg.obj" \
	"$(INTDIR)\PtrDlg.obj" \
	"$(INTDIR)\Rectdlg.obj" \
	"$(INTDIR)\RichCombo.obj" \
	"$(INTDIR)\RichEditEx.obj" \
	"$(INTDIR)\RuleQDlg.obj" \
	"$(INTDIR)\SolveVarDlg.obj" \
	"$(INTDIR)\Splash.obj" \
	"$(INTDIR)\StageObj.obj" \
	"$(INTDIR)\Stdafx.obj" \
	"$(INTDIR)\SymbolMenu.obj" \
	"$(INTDIR)\SysDlg.obj" \
	"$(INTDIR)\TabView.obj" \
	"$(INTDIR)\TaskDlg.obj" \
	"$(INTDIR)\TraceDlg.obj" \
	"$(INTDIR)\VariableDlg.obj" \
	"$(INTDIR)\VarView.obj" \
	"$(INTDIR)\VecAVDlg.obj" \
	"$(INTDIR)\VecCpDlg.obj" \
	"$(INTDIR)\VecDlg.obj" \
	"$(INTDIR)\VwOptDlg.obj" \
	"$(INTDIR)\wbrowser.obj" \
	"$(INTDIR)\FBD.res" \
	"$(INTDIR)\EXHintDg.obj" \
	"$(INTDIR)\Exp2Dlg.obj" \
	"$(INTDIR)\Exp4Dlg.obj" \
	"$(INTDIR)\ExpBdyDg.obj" \
	"$(INTDIR)\EXPlanVw.obj" \
	"$(INTDIR)\ExpLawDlg.obj" \
	"$(INTDIR)\EXTxtDlg.obj" \
	"$(INTDIR)\EXView.obj" \
	"$(INTDIR)\PlanObj.obj" \
	"$(INTDIR)\PlanStrs.obj" \
	"$(INTDIR)\PlanView.obj" \
	"$(INTDIR)\stepdlg.obj" \
	"$(INTDIR)\TemplateDlg.obj" \
	"$(INTDIR)\DoneDlg.obj"

"$(OUTDIR)\Fbd-woz.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

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


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("Fbd.dep")
!INCLUDE "Fbd.dep"
!ELSE 
!MESSAGE Warning: cannot find "Fbd.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "FBD - Win32 Release" || "$(CFG)" == "FBD - Win32 Debug" || "$(CFG)" == "FBD - Win32 Atlas Debug" || "$(CFG)" == "FBD - Win32 Atlas Release" || "$(CFG)" == "FBD - Win32 TCP Debug" || "$(CFG)" == "FBD - Win32 TCP Release"
SOURCE=".\helpifc-tcp.cpp"

!IF  "$(CFG)" == "FBD - Win32 Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"


"$(INTDIR)\helpifc-tcp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


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


SOURCE=.\DemoDlg.cpp

"$(INTDIR)\DemoDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


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

"$(INTDIR)\donedlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\DoneDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

"$(INTDIR)\EXHintDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXHintDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXHintDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXHintDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXHintDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXHintDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp2Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Exp4Dlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\expbdydg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpBdyDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpBdyDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpBdyDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpBdyDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpBdyDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpBdyDg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXPlanVw.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\ExpLawDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\expmenu.rc
SOURCE=.\Example\extxtdlg.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXTxtDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXTxtDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXTxtDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXTxtDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXTxtDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXTxtDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\exview.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\EXView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
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


SOURCE=.\PicCtrl.cpp

"$(INTDIR)\PicCtrl.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Example\planobj.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanObj.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\planstrs.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanStrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanStrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanStrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanStrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanStrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanStrs.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\Example\planview.cpp

!IF  "$(CFG)" == "FBD - Win32 Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "USNA_EVAL" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "USNA_EVAL" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 Atlas Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Debug"

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\PlanView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
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


SOURCE=.\ProblemSet.cpp

"$(INTDIR)\ProblemSet.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ProbSetEditView.cpp

"$(INTDIR)\ProbSetEditView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\ProbSetView.cpp

"$(INTDIR)\ProbSetView.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PropertyDlg.cpp

"$(INTDIR)\PropertyDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\PtrDlg.cpp

"$(INTDIR)\PtrDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\Rectdlg.cpp

"$(INTDIR)\Rectdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\Stdafx.obj"	"$(INTDIR)\Fbd.pch" : $(SOURCE) "$(INTDIR)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yc"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\stepdlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\SymbolMenu.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

CPP_SWITCHES=/nologo /MDd /W3 /Gm /GX /ZI /Od /I "Example" /I "." /D "_DEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "FBD - Win32 TCP Release"

CPP_SWITCHES=/nologo /MD /W3 /GX /O1 /I "Example" /I "." /D "NDEBUG" /D "_AFXDLL" /D "WIN32" /D "_WINDOWS" /D "_MBCS" /D "FBD_SCROLL_VIEW" /D "EX_SCROLL_VIEW" /D "EQ_RICHEDIT" /D "ATLAS" /D "HELPIFC_TCP" /Fp"$(INTDIR)\Fbd.pch" /Yu"stdafx.h" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

"$(INTDIR)\TemplateDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\TraceDlg.cpp

"$(INTDIR)\TraceDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


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


SOURCE=.\VwOptDlg.cpp

"$(INTDIR)\VwOptDlg.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\wbrowser.cpp

"$(INTDIR)\wbrowser.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\Fbd.pch"


SOURCE=.\FBD.rc

"$(INTDIR)\FBD.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)



!ENDIF 

