// MainFrm.cpp : implementation of the CMainFrame class
//
// $Id: Mainfrm.cpp,v 1.3 2006/07/27 23:04:06 anders Exp $
    
#include "stdafx.h"
#include "dde.h"			// for OnDdeCommand override 
#include "FBD.h"
#include "helpifc.h"		// for handling procedural help requests
#include "history.h"		// required before logging dialogs 
#include "FBDDoc.h"			// required before FBDView, urgh.
#include "FBDView.h"		// for invalidating on palette changes
#include "ChildFrm.h"
#include "MainFrm.h"
#include "HintDlg.h"
#include "HintView.h"
#include "ChatView.h"
#include "PlayDlg.h"
#include "DemoDlg.h"
#include "PtrDlg.h"
#include "Splash.h"
#include "TaskDlg.h"
#include "CommentDlg.h"
#include "mymenu.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
        
/////////////////////////////////////////////////////////////////////////////
// CMainFrame
    
IMPLEMENT_DYNAMIC(CMainFrame, CMDIFrameWnd)
    
BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWnd)
   	ON_WM_QUERYNEWPALETTE()
   	ON_WM_PALETTECHANGED()
   	ON_UPDATE_COMMAND_UI(ID_INDICATOR_TIME, OnUpdateTime)
	ON_UPDATE_COMMAND_UI(ID_INDICATOR_SCORE, OnUpdateScore)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_COMMAND(ID_VIEW_PLAYCONTROL, OnViewPlaycontrol)
	ON_UPDATE_COMMAND_UI(ID_VIEW_PLAYCONTROL, OnUpdateViewPlaycontrol)
	ON_WM_TIMER()
	ON_WM_ENTERIDLE()
	ON_COMMAND(ID_PLAYER_PLAY, OnPlayerPlay)
	ON_COMMAND(ID_PLAYER_STOP, OnPlayerStop)
	ON_COMMAND(ID_PLAYER_FASTFORWARD, OnPlayerFastforward)
	ON_COMMAND(ID_PLAYER_SKIP, OnPlayerSkip)
	ON_COMMAND(ID_PLAYER_PAUSE, OnPlayerPause)
	ON_UPDATE_COMMAND_UI(ID_PLAYER_PLAY, OnUpdatePlayerPlay)
	ON_UPDATE_COMMAND_UI(ID_PLAYER_PAUSE, OnUpdatePlayerPause)
	ON_UPDATE_COMMAND_UI(ID_PLAYER_FASTFORWARD, OnUpdatePlayerFastforward)
	ON_UPDATE_COMMAND_UI(ID_PLAYER_STOP, OnUpdatePlayerStop)
	ON_COMMAND(ID_FILE_REPLAYLOG, OnFileReplaylog)
	ON_WM_SETCURSOR()
	ON_COMMAND(ID_CALCULATOR, OnCalculator)
	ON_COMMAND(ID_GET_PROC_HELP, OnGetProcHelp)
	ON_WM_CLOSE()
	ON_WM_INITMENUPOPUP()
	ON_COMMAND(ID_PHYS_REVIEW_EQN, OnPhysReviewEqn)
	ON_COMMAND(ID_FILE_SHOWDEMO, OnFileShowdemo)
	ON_COMMAND(ID_VIEW_PTR_DLG, OnViewPtrDlg)
	ON_UPDATE_COMMAND_UI(ID_FOLLOWUP_EXPLAIN, OnUpdateFollowupExplain)
	ON_UPDATE_COMMAND_UI(ID_FOLLOWUP_HOW, OnUpdateFollowupHow)
	ON_UPDATE_COMMAND_UI(ID_FOLLOWUP_WHY, OnUpdateFollowupWhy)
	ON_UPDATE_COMMAND_UI(ID_FOLLOWUP_HIDE, OnUpdateFollowupHide)
	ON_COMMAND(ID_PLAYER_REPLAY_HELP, OnPlayerReplayHelp)
	ON_UPDATE_COMMAND_UI(ID_PLAYER_REPLAY_HELP, OnUpdatePlayerReplayHelp)
	ON_COMMAND(ID_PLAYER_QUIT_DEMO, OnPlayerQuitDemo)
	ON_WM_ACTIVATEAPP()
	ON_UPDATE_COMMAND_UI(ID_PLAYER_GOTO, OnUpdatePlayerGoto)
	ON_UPDATE_COMMAND_UI(ID_VIEW_AUTHORBAR, OnUpdateViewAuthorbar)
	ON_COMMAND(ID_TEXTBOOK, OnTextbook)
	ON_UPDATE_COMMAND_UI(ID_GET_PROC_HELP, OnUpdateGetProcHelp)
	ON_COMMAND(ID_COMMENT, OnComment)
	ON_COMMAND(ID_PLAYER_STEP, OnPlayerStep)
	ON_UPDATE_COMMAND_UI(ID_PLAYER_STEP, OnUpdatePlayerStep)
	ON_COMMAND(ID_GREEK, OnGreek)
	ON_UPDATE_COMMAND_UI(ID_GREEK, OnUpdateGreek)
	ON_COMMAND(ID_PLAYER_SNAPSHOT, OnPlayerSnapshot)
	ON_UPDATE_COMMAND_UI(ID_PLAYER_SNAPSHOT, OnUpdatePlayerSnapshot)
	ON_COMMAND(ID_FOLLOWUP_EXPLAIN, OnFollowupExplain)
	ON_COMMAND(ID_FOLLOWUP_HOW, OnFollowupHow)
	ON_COMMAND(ID_FOLLOWUP_WHY, OnFollowupWhy)
	ON_COMMAND(ID_FOLLOWUP_HIDE, OnFollowupHide)
	ON_COMMAND(ID_WEB_SUPPORT, OnWebSupport)
	ON_UPDATE_COMMAND_UI(ID_TEXTBOOK, OnUpdateTextbook)
 	ON_WM_DRAWITEM()
	ON_WM_MEASUREITEM()
	ON_COMMAND_EX(ID_VIEW_AUTHORBAR, OnBarCheck)
	ON_COMMAND_EX(ID_VIEW_EQBAR, OnBarCheck)
	ON_UPDATE_COMMAND_UI(ID_VIEW_EQBAR, OnUpdateControlBarMenu)
	ON_UPDATE_COMMAND_UI(ID_PHYS_REVIEW_EQN, OnUpdateTextbook)
	ON_COMMAND(ID_HELP_EMAIL, OnHelpEmail)
	//}}AFX_MSG_MAP
	// Global help commands
	ON_COMMAND(ID_HELP_FINDER, CMDIFrameWnd::OnHelpFinder)
	ON_COMMAND(ID_HELP, CMDIFrameWnd::OnHelp)
	ON_COMMAND(ID_CONTEXT_HELP, CMDIFrameWnd::OnContextHelp)
	ON_COMMAND(ID_DEFAULT_HELP, CMDIFrameWnd::OnHelpFinder)
	// Modeless dialog notifications
	ON_MESSAGE(WM_CLOSE_PLAY_DLG, OnClosePlayDlg)
	ON_MESSAGE(WM_CLOSE_DEMO_DLG, OnCloseDemoDlg)
	// To override default MFC implementation
	ON_MESSAGE(WM_DDE_EXECUTE, OnDDEExecute)
	// To handle drop-down button on solve-for toolbar command
	ON_NOTIFY(TBN_DROPDOWN, IDW_EQBAR, OnDropSolveFor)
END_MESSAGE_MAP()
    
static UINT indicators[] =
{
	ID_SEPARATOR,   // status line indicator
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};
    
/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

// Main frame is manager of a possibly-shown modeless dialog tool for controlling 
// log playback. To manage the modeless dialog, we follow example from
// the Kruglinski book. We here allocate and construct the C++ CDialog 
// object, but do *not* create an associated Windows dialog window. 
// That is done when the tool is popped up from a menu command. When the user
// dismisses it, it sends an application-defined message to the frame, which
// then destroys the Windows window (below). The C++ object persists for the 
// lifetime of the frame. 
//
CMainFrame::CMainFrame()
{
	// state flags:
	m_bInDdeWait = FALSE;
	m_bClosing = FALSE;
	m_bClearHintOnDde = TRUE;	// default is to clear it, right for most calls.
	//
	// modeless windows (floating tools) we control:
	//
	m_pPlayDlg = new CPlayDlg(this);
//	m_pHintWnd = new CHintDlg(this);
	// Following treated differently, construct C++ obj as needed and
	// keep it only as long as window is showing. No big difference;
	// either way, GetSafeHwnd works to test if window is created.
	// Keeping C++ obj around lets us save persisting member state if we
	// DDX it into members, although we don't currently use any. (Could also
	// keep Windows window around but invisible for that matter.)
	m_pDemoDlg = NULL;					
	m_pPtrDlg = NULL;
}
    
CMainFrame::~CMainFrame()
{
	// clean up  optionally created C++ objects for floating tools.
	if (m_pPlayDlg) delete m_pPlayDlg;	
	if (m_pDemoDlg) delete m_pDemoDlg;
	if (m_pPtrDlg) delete m_pPtrDlg;
}

// In Windows, all windows are  created as instances of registered named WNDCLASS's.
// We want to set a custom class name on our frame's Windows WNDCLASS so that we
// and other processes can easily locate it by class name with the FindWindow API.
// Normally WNDCLASS registration is done as needed by MFC just before window 
// creation. However, for frames created by LoadFrame, PreCreateWindow is 
// evidently called *twice*: first time from GetIconWndClass *before* a WNDCLASS 
// has been registered (apparently to register a variant WNDCLASS with default 
// icon); second time *after* the WNDCLASS for the frame has been registered. 
//
// Here we hook 2nd call (has non-NULL class name), read the attributes of
// the WNDCLASS MFC has registered; register new WNDCLASS with same attributes 
// but different name, and modify the CREATESTRUCT to use that class name.

const LPCTSTR FBD_CLASSNAME = _T("FBD");	// custom name for our frame window WNDCLASS
 
BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	if (cs.lpszClass == NULL) // MFC has not yet registered WNDCLASS for frame
		return CMDIFrameWnd::PreCreateWindow(cs);
	
	// Else WNDCLASS class name is set coming in
	// Get info for frame class that MFC has registered.
	WNDCLASSEX wc;
	wc.cbSize = sizeof(WNDCLASSEX);
	GetClassInfoEx(AfxGetInstanceHandle(), cs.lpszClass, &wc);

	// Modify wndclass name 
	wc.lpszClassName = FBD_CLASSNAME;
		
	// Register new class with custom name
	VERIFY(RegisterClassEx(&wc));

	// And modify windows creation params to use new class.
	cs.lpszClass = FBD_CLASSNAME;
	return TRUE;
}

// static function to find an existing instance of running FBD mainframe window
// Intended to be used before creating mainframe to check for other instance.
// (if used after, it could return this mainframe window).
CWnd* CMainFrame::FindFBDWindow()
{
	return FindWindow(FBD_CLASSNAME, NULL);
}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	// CG: This line was added by the Palette Support component
	m_pPalette = NULL;
	if (CMDIFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

	// Set frame to allow docking on any border.
	EnableDocking(CBRS_ALIGN_ANY);

	// Create main tool bar
	// Registry setting enables variant toolbar for experimenting
	int nToolbarID = IDR_MAINFRAME;
	if (theApp.GetProfileInt("Settings", "Experiment", 0))
		nToolbarID = IDR_MAINFRAME_EXP;

	if (!m_wndToolBar.Create(this) ||		
		!m_wndToolBar.LoadToolBar(nToolbarID))
	{
		TRACE0("Failed to create toolbar\n");
		return -1;  // fail to create
	}
	// TODO: Remove this if you don't want tool tips or a resizeable toolbar
	m_wndToolBar.SetBarStyle(m_wndToolBar.GetBarStyle() |
		CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC);

	// Make toolbar dockable on any border and actually dock it 
	m_wndToolBar.EnableDocking(CBRS_ALIGN_ANY);
	DockControlBar(&m_wndToolBar); // apparently defaults to docking at top

	// Create status bar
	if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE0("Failed to create status bar\n");
		return -1;  // fail to create
	}

	// Create optional dockable author/drawing tool bar, initially hidden.
	//
	// The one-argument create call for main toolbar above uses the default child
	// window ID parameter, AFX_IDW_TOOLBAR. For other bars, we need to send all 
	// parameters so as to specify a different child window id. NOTE: although this
	// fact is undocumented, the child window ids for toolbars should be chosen in a 
	// special range to interact correctly with the MFC print preview support.
	// Turns out a magic range of 32 control bar IDs starting with AFX_IDW_TOOLBAR 
	// is treated specially when print preview code saves and restores Mainframe 
	// toolbar state -- it uses a 32-bit mask to save visible bit for bars in this range.
	//.A peek into Afxres.h suggests MFC reserves first 4 IDs in this range 
	// (e.g. default status bar ID AFX_IDW_STATUSBAR = AFX_IDW_TOOLBAR + 1)  
	// and also defines the magic ids AFX_IDW_DOCKBAR_LEFT/RIGHT/etc for docking bays
	// as the last 5 in this range.  Our IDs were chosen to try to ensure our 
	// bar is in the magic range while avoiding conflicts with the framework-defined IDs.
	if (!m_wndAuthorBar.Create(this, WS_CHILD | 
		    CBRS_HIDE_INPLACE |CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC, 
			(UINT) IDW_AUTHORBAR) 
		|| !m_wndAuthorBar.LoadToolBar(IDR_AUTHORBAR))
	{
		TRACE0("Failed to create drawing toolbar\n");
		return -1;  // fail to create
	}
	// Invisible bar dockable anywhere; initially on top at right of mainframe bar
	m_wndAuthorBar.EnableDocking(CBRS_ALIGN_ANY);
	DockControlBarLeftOf(&m_wndAuthorBar, &m_wndToolBar);
	// Now possible to start up in author mode w/command line flag, so show as needed.
	if (theApp.m_bAuthorMode)
		ShowAuthorBar();

	if (!m_wndEQBar.Create(this, WS_CHILD | WS_VISIBLE |
			CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC, 
			(UINT) IDW_EQBAR) 
	    || !m_wndEQBar.LoadToolBar(IDR_EQBAR))
	{
			TRACE0("Failed to create equation toolbar\n");
			return -1;
	}
	// Modify styles to put dropdown button on SOLVE_FOR command
	m_wndEQBar.GetToolBarCtrl().SetExtendedStyle(TBSTYLE_EX_DRAWDDARROWS);
	TBBUTTONINFO tbi;
	tbi.dwMask= TBIF_STYLE;
	tbi.cbSize= sizeof(TBBUTTONINFO);
	m_wndEQBar.GetToolBarCtrl().GetButtonInfo(ID_SOLVEFOR, &tbi);
	tbi.fsStyle |= TBSTYLE_DROPDOWN;
	m_wndEQBar.GetToolBarCtrl().SetButtonInfo(ID_SOLVEFOR, &tbi);
	// Initially docked on top at right of mainframe bar
	m_wndEQBar.EnableDocking(CBRS_ALIGN_ANY);
	DockControlBarLeftOf(&m_wndEQBar, &m_wndToolBar);

	// Create Greek symbol dialog (initially hidden).
	if (!m_dlgSymbolMenu.Create(IDD_GREEKBAR, this))
			TRACE0("Failed to create symbol menu\n");

	// Following code appends a time pane to the original status bar
	// CG: The following block was inserted by 'Status Bar' component.
	{
		// Find out the size of the static variable 'indicators' defined
		// by AppWizard and copy it
		int nOrigSize = sizeof(indicators) / sizeof(UINT);

		UINT* pIndicators = new UINT[nOrigSize + 3];
		memcpy(pIndicators, indicators, sizeof(indicators));
    
		// Call the Status Bar Component's status bar creation function
		if (!InitStatusBar(pIndicators, nOrigSize, 60))
		{
			TRACE0("Failed to initialize Status Bar\n");
			return -1;
		}
		delete[] pIndicators;
	}

	// Mainframe shows the splash screen on create:
	// CG: The following line was added by the Splash Screen component.
	CSplashWnd::ShowSplashScreen(this);
	return 0;
}

// DockControlBarLeftOf -- routine for docking control bars side by side 
//                         taken from MFC docktool sample.
// Name means: dock first bar wrt already-placed second bar at LeftOf it.
void CMainFrame::DockControlBarLeftOf(CToolBar* Bar,CToolBar* LeftOf)
{
	CRect rect;
	DWORD dw;
	UINT n;

	// get MFC to adjust the dimensions of all docked ToolBars
	// so that GetWindowRect will be accurate
	RecalcLayout();
	LeftOf->GetWindowRect(&rect);
	rect.OffsetRect(1,0);
	dw=LeftOf->GetBarStyle();
	n = 0;
	n = (dw&CBRS_ALIGN_TOP) ? AFX_IDW_DOCKBAR_TOP : n;
	n = (dw&CBRS_ALIGN_BOTTOM && n==0) ? AFX_IDW_DOCKBAR_BOTTOM : n;
	n = (dw&CBRS_ALIGN_LEFT && n==0) ? AFX_IDW_DOCKBAR_LEFT : n;
	n = (dw&CBRS_ALIGN_RIGHT && n==0) ? AFX_IDW_DOCKBAR_RIGHT : n;

	// When we take the default parameters on rect, DockControlBar will dock
	// each Toolbar on a seperate line.  By calculating a rectangle, we in effect
	// are simulating a Toolbar being dragged to that location and docked.
	DockControlBar(Bar,n,&rect);
}

    
// Handle WM_CLOSE sent to main window to initiate application shutdown. The default 
// ID_EXIT command implementation just sends WM_CLOSE to main window; can also be 
// generated if user hits close button or from sys menu or standard accelerator.(?}
//
// Important: In PrintPreview mode the parent class will *not* initiate shutdown on this,
// but will rather just close the preview mode view and return to previous view. We
// have to take care to avoid setting the m_bClosing flag in this case, because the
// subsequent processing will get confused and Andes appears to hang.
void CMainFrame::OnClose() 
{
	// copied from CFrameWnd::OnClose -- if a custom CloseProc has been registered (as 
	// is done by PrintPreview mode, then don't close. No need to call base class in 
	// this case, it's all the base class does as well.
	if (m_lpfnCloseProc != NULL && !(*m_lpfnCloseProc)(this))
		return;

	// could also use following to test for Preview mode
	// CView* pView = (CPreviewView*) pFrameWnd->GetDlgItem(AFX_IDW_PANE_FIRST);
	// if (pView->IsKindOf(RUNTIME_CLASS(CPreviewView))
   	LogEventf(EV_CLOSE_APP, "");

  	// Set flag to indicate app is in process of shutting down. 
  	// This is used to suppress normal return to task choice dialog after doc close
   	m_bClosing = TRUE;	
    
  	// Note: base class implementation will use CWinApp::SaveAllModified to prompt user to
  	// save changes to any open documents. Users get a "cancel" option; if cancelled or save 
  	// fails. mainframe close/app shutdown is aborted. So we can't know for sure that app is 
  	// *really* shutting down until after all modified docs checked/saved. Therefore, we hook
  	// app's SaveAllModified method to clear the Frame's m_bClosing flag if the shutdown is 
  	// aborted.
  	
  	// Save persistent frame window state vars in registry
  	SaveWindowState();
    	
 /*  doing log upload from here seems to be causing crash on shutdown on some machines
   	// Finish history log
   	HistoryFileEnd();
    
   	// Try to upload a copy for experiment
   	HistoryUpload(); // Can't run MFC dialog without main window here
   */
 	// Base class starts the real work of shutting down.
 	// check if ok to close current document; Example helper may override.
 	if ((theApp.GetDocument() != NULL)//document already closed
 		&&(((CFBDDoc*)theApp.GetDocument())->m_nProblemType == PROB_EXAMPLE))
 	{	
 		if (!theApp.m_bAuthorMode){
 			if (!((CFBDDoc*)theApp.GetDocument())->HelpAllowClose())
 				return;
 		}
 	}
 
 	// Base class method will call SaveAllModified.
  	CMDIFrameWnd::OnClose();
}
    
/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics
    
#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CMDIFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CMDIFrameWnd::Dump(dc);
}
    
#endif //_DEBUG
    
/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers
    
void CMainFrame::ShowAuthorBar(BOOL bShow)
{
	ShowControlBar((CControlBar*) &m_wndAuthorBar, bShow, FALSE);
}

//
// For displaying current angle in status bar
// 
void CMainFrame::SetAngleText(LPCTSTR str)
{
	m_wndStatusBar.SetPaneText(0, str);		// Use prompt field at index 0
}

// 
// For displaying current session time in status bar clock:
// Handler for update message for this pane.  It is executed whenever ON_UPDATE_COMMAND_UI 
// happens, normally during MFC idle time processing.
//
void CMainFrame::OnUpdateTime(CCmdUI* pCmdUI)
{
	// CG: This function was inserted by 'Status Bar' component.

	// Get current date and format it
/*
	CTime time = CTime::GetCurrentTime();
	CString strTime = time.Format(_T("%#I:%M :%S "));
	strTime = (time.GetHour() < 12 ? _T("") : _T(""))+ strTime +(time.GetHour() < 12 ? _T(""):_T("  "));
*/
	// changed above to use history time = seconds since startup
	CString strTime = HistTimeSpan().Format("%H:%M:%S");

	// following is cheap way to show that app is in log playback mode.
	// Time is still real session time, not log time.
	if (LogPlayerInPlayback())
		strTime += " (Log)";
	
	/* strTime.Format("%d", HistTime()); */

	// BLOCK: compute the width of the date string
	CSize size;
	{
		HGDIOBJ hOldFont = NULL;
		HFONT hFont = (HFONT)m_wndStatusBar.SendMessage(WM_GETFONT);
		CClientDC dc(NULL);
		if (hFont != NULL) 
			hOldFont = dc.SelectObject(hFont);
		size = dc.GetTextExtent(strTime);
		if (hOldFont != NULL) 
			dc.SelectObject(hOldFont);
	}

	// Update the pane to reflect the current time
	UINT nID, nStyle;
	int nWidth;
	m_wndStatusBar.GetPaneInfo(m_nTimePaneNo, nID, nStyle, nWidth);
	m_wndStatusBar.SetPaneInfo(m_nTimePaneNo, nID, nStyle, size.cx);
	pCmdUI->SetText(strTime);
	pCmdUI->Enable(TRUE);    
}

void CMainFrame::OnUpdateScore(CCmdUI* pCmdUI)
{
	// Get score and format pane display contents
	CFBDDoc* pDoc = theApp.GetCurrentProblem();
	if (! pDoc) return;
	/* float fScore;
	sscanf(pDoc->m_strScore, "%f", &fScore); */
	CString strScore; 
	strScore.Format("SCORE: %s ", pDoc->m_strScore);

	// compute the width of the score string
	CSize size;
	HGDIOBJ hOldFont = NULL;
	HFONT hFont = (HFONT)m_wndStatusBar.SendMessage(WM_GETFONT);
	CClientDC dc(NULL);
	if (hFont != NULL) 
		hOldFont = dc.SelectObject(hFont);
	size = dc.GetTextExtent(strScore);
	if (hOldFont != NULL) 
		dc.SelectObject(hOldFont);

	// Update the pane to reflect the current score
	UINT nID, nStyle;
	int nWidth;
	m_wndStatusBar.GetPaneInfo(m_nScorePaneNo, nID, nStyle, nWidth);
	m_wndStatusBar.SetPaneInfo(m_nScorePaneNo, nID, nStyle, size.cx);
	pCmdUI->SetText(strScore);
	pCmdUI->Enable(TRUE);    
}

BOOL CMainFrame::InitStatusBar(UINT *pIndicators, int nSize, int nSeconds)
{
	// CG: This function was inserted by 'Status Bar' component.

	// Create an index for the TIME pane
	m_nTimePaneNo = nSize++;
	pIndicators[m_nTimePaneNo] = ID_INDICATOR_TIME;

	// Create an index for the SCORE pane
	m_nScorePaneNo = nSize++;
	pIndicators[m_nScorePaneNo] = ID_INDICATOR_SCORE;

	// TODO: Select an appropriate time interval for updating
	// the status bar when idling.
	// (Periodic timer messages force idle updating to occur repeatedly 
	// even during intervals with no user input.)
	nSeconds = 1;
	// set in frame, not status bar, so we can do other things periodically.
	// m_wndStatusBar.SetTimer(0x1000, nSeconds * 1000, NULL);
	SetTimer(0x1000, nSeconds * 1000, NULL);
	
	return m_wndStatusBar.SetIndicators(pIndicators, nSize);
}

void CMainFrame::OnTimer(UINT nIDEvent) 
{
	// Currently nothing to do. Periodic timer events are requested so that 
	// on-screen session clock pane will be updated. This happens during
	// the framework's idle-time processing through the updateUI mechanism.
	CMDIFrameWnd::OnTimer(nIDEvent);

	// if DDEML is running its modal loop we force a UI update periodically
	// so that status bar clock will continue to update (it doesn't notify us on idle!).
	if (m_bInDdeWait)
		AfxGetApp()->OnIdle(0);
}
 
// (This technique taken from one of the newsgroups.) When inside a dialog's
// modal loop, the dialog sends WM_ENTERIDLE messages to the parent when 
// entering the idle state. This code forces the application's OnIdle 
// routine to be called to do its idle time processing even during modal 
// dialogs.
//
// The main effect for us is that the clock on the status bar pane can
// continue to be updated as part of the idle-time's UPDATE_COMMAND_UI
// processing. It also allows for command UI updating while in modal dlg,
// although not clear if we need that for anything.

void CMainFrame::OnEnterIdle(UINT nWhy, CWnd* pWho) 
{
	CMDIFrameWnd::OnEnterIdle(nWhy, pWho);
	AfxGetApp()->OnIdle(0);
}

// Log changes in application activation
void CMainFrame::OnActivateApp(BOOL bActive, HTASK hTask) 
{
	CMDIFrameWnd::OnActivateApp(bActive, hTask);
	
	if (bActive) {
		LogEventf(EV_APP_ACTIVATE, "");
		theApp.MakeSound(sndActivateApp);
	} else {
		LogEventf(EV_APP_DEACTIVATE, "");
		theApp.MakeSound(sndDeactivateApp);
	}
}
   
////////////////////////////////////////////////////////////////////////////////////
//
// Log player UI management: 
// Mainframe manages the floating tool used to control to the log player task 
// during playback mode.
//
// ISSUES: We tried using a floating dialog bar. This gets the command buttons wired in 
// to the application's command route and update mechanism. However, as an owned 
// popup of the frame, the framework will automatically disable it whenever a 
// modal dialog is put up. (?? And whenever the mainframe is disabled, as 
// it is during log playback? Not sure.) But that means user can't get control 
// of the log playback in this state.
//
// Because the player control must respond to user input even when the rest of
// the application is disabled, we use a modeless dialog and do *not* "parent" it 
// (i.e. set its ownership) to the mainframe; rather, to the desktop window. We 
// also make it a topmost window to ensure it's always visible. We handle the hookup 
// to the command handlers in the dialog class's OnCmdMsg, and have the player 
// explictly notify the dialog when it should to update the command UI's 
// via UpdateDialogControls.
//
// When the user interacts with the log player it becomes the active window in the 
// application (normally shown by blue title bar).  There are some issues relating
// to the way MFC uses and updates the active window. For example, MFC always 
// centers modal dialogs with respect to the active window. which could cause them
// to come up in an odd place during playback. This is no longer a problem in most
// cases since we now place most of our dialogs explicitly, and can replay the 
// positioning from the log in any case. 
// 
// Modal dialogs attempt to restore activation to the last active 
// popup on closing, and Windows must transfer activation somewhere on close 
// of an active window. Activation cannot be set to a disabled window, so we have 
// to make sure it has somewhere to go when we close the floating tool, or another
// application will be activated.
//
// It might be best to use a separate thread with its own UI to control the log 
// playback, to minimize interference between user's control of the playback 
// process and what is going on in the controlled application.But that would bring in problems with inter-thread 
// communication with C++ objects (see Prosise book). Another possibility is a
// completely separate application process which controls the application via DDE.
//
void CMainFrame::OnViewPlaycontrol() // toggles visibility of playback control
{
	if (m_pPlayDlg->GetSafeHwnd() == 0) // No Window handle -> modeless tool is not up
	{
		// ensure C++ obj is constructed.
		if (! m_pPlayDlg) m_pPlayDlg = new CPlayDlg(this);

		// create and displays the modeless dialog window
		// If it's parented to the Mainframe, then it will get disabled whenever
		// the mainframe is, and user won't be able to control the log playback
		// during those intervals. So we make it a top-level window by parenting
		// it to the desktop. 
		CWnd* pWndDesktop = CWnd::FromHandle(::GetDesktopWindow());
		m_pPlayDlg->Create(pWndDesktop);
		
		// place it at appropriate location on screen. For now
		// we put it along top of screen, with left midway across (indep of frame). 
		CClientDC dc(this);
		int xMidScreen = dc.GetDeviceCaps(HORZRES)/2;

		// make it a topmost window to ensure always visible, and show it.
		m_pPlayDlg->SetWindowPos(&wndTopMost, xMidScreen, 0, /*ignored:*/ 0, 0, 
			                      SWP_NOSIZE | SWP_SHOWWINDOW);
	} 
	else // Tool is already up
	{
		delete m_pPlayDlg;	// destructor destroys Windows window
		m_pPlayDlg = NULL;

		LogPlayerSetUI(NULL);// to be safe
		/*
		m_pPlayDlg->DestroyWindow();
		// destroys window, but C++ object sticks around */
	}
}
    
void CMainFrame::OnUpdateViewPlaycontrol(CCmdUI* pCmdUI) 
{
	// check menu item if tool is up
	pCmdUI->SetCheck (m_pPlayDlg->GetSafeHwnd() != 0);
}
    
LONG CMainFrame::OnClosePlayDlg(UINT wParam, LONG lParam)
{
	// modeless dialog dismissed by user

	// should have generated a stop command, following is just to be sure
	LogPlayerStop();

	// m_pPlayDlg->DestroyWindow();
	delete m_pPlayDlg;		// destructor destroys Windows window
	m_pPlayDlg = NULL;
	
	LogPlayerSetUI(NULL);	// to be safe
	return 0L;
}

LONG CMainFrame::OnCloseDemoDlg(UINT wParam, LONG lParam)
{
	// Notification that modeless demo player was dismissed by user
	// This message not used any more; dialog sends us a QUIT_DEMO command instead.

	// should have generated a stop command, following is just to be sure
	LogPlayerStop();

	// m_pDemoDlg->DestroyWindow();
	delete m_pDemoDlg;		// destructor destroys Windows window
	m_pDemoDlg = NULL;

	LogPlayerSetUI(NULL);	// to be safe

	return 0L;
}
  
//
// Following handle log playback control commands by calling appropriate routine 
// in the log player. These commands may be routed to us from the floating player controls.
// We also provide update handlers for the state of the player, for use by
// the floating tools.
void CMainFrame::OnPlayerPlay() // start playback of log file chosen from dialog
{
	CString strPathName;

	// Retrieve log filename currently entered in player control.
	m_pPlayDlg->GetPathName(strPathName);

	// Here assuming it's from Log Player, not Demo player!
	theApp.StartPlaybackMode(strPathName, m_pPlayDlg);
}

void CMainFrame::OnPlayerStop() // stop playback process
{
	// note this should be safe even if not currently playing.
	// floating tools generate it on tool close, just to be safe.
    LogPlayerStop();
	
}

void CMainFrame::OnPlayerFastforward() // toggle FF flag
{
    LogPlayerToggleFF();
}
    
void CMainFrame::OnPlayerSkip()		// FF to specified time
{
	// Not yet implemented
}

void CMainFrame::OnPlayerPause()	// pause or resume playback
{
    LogPlayerPauseResume();
}

void CMainFrame::OnPlayerReplayHelp() // toggle replay help flag
{
	LogPlayerToggleCallHelp();
}

void CMainFrame::OnPlayerStep() 
{
	LogPlayerStep();
}

// command to save a "snapshot" file with the current state, for later viewing/printing
// This command could be available at any time, but currently only fired from log player.
void CMainFrame::OnPlayerSnapshot() 
{
	// For now, we just do a SaveCopyAs operation with a standard filename
	// We don't want to prompt user with dialog since if in paused log playing,
	// user input is only let through to the player control. 
	// !! Will be a problem if save fails and message box comes up to report --
	// no way to send input to dismiss it.
	LogPlayerSnapshot();	
}

void CMainFrame::OnUpdatePlayerSnapshot(CCmdUI* pCmdUI) 
{
	// For now, only use it from log playback . logfile name is used in 
	// automatically generated snapshot file name).
	pCmdUI->Enable(LogPlayerInPlayback() && theApp.GetCurrentProblem());
	// Could enable it whenever there's a current problem open.
}

// Playback CmdUI must both represent current playback state and enabling of commands 
// for changing state, i.e. play button down means "is playing", Pause down means 
// "is paused",etc. For that reason, they are actually radio buttons with the pushbutton
// appearance, not command push buttons (which don't handle SetCheck); or else
// check boxes which display current option value and are mapped to toggle
// commands on clicks. Exception: the stop button, which is just a command button.
//
void CMainFrame::OnUpdatePlayerPlay(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(LogPlayerInPlayback());
	pCmdUI->Enable(! LogPlayerInPlayback());
}

void CMainFrame::OnUpdatePlayerPause(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(LogPlayerPaused());
	pCmdUI->Enable(LogPlayerInPlayback());
	pCmdUI->SetText(/* LogPlayerPaused() ? "Continue" :*/ "Pause");
}
    
void CMainFrame::OnUpdatePlayerFastforward(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(LogPlayerFF());
}
    
void CMainFrame::OnUpdatePlayerStop(CCmdUI* pCmdUI) 
{
    pCmdUI->Enable(LogPlayerInPlayback());
}

void CMainFrame::OnUpdatePlayerReplayHelp(CCmdUI* pCmdUI) 
{
	// display is inverse of flag value
	pCmdUI->SetRadio(!LogPlayerCallHelp());
	// user only gets a choice if real help system (not simulated one)
	// is currently connected. !!! With deferred connection (done as
	// needed on problem open), won't be connected if haven't opened any problem.
	// Maybe want to test if help system is running instead.
	pCmdUI->Enable(HelpSystemReallyConnected());
}

void CMainFrame::OnUpdatePlayerGoto(CCmdUI* pCmdUI) 
{
	pCmdUI->SetRadio(LogPlayerInGoto());
	pCmdUI->Enable(! LogPlayerInGoto());
}

void CMainFrame::OnUpdatePlayerStep(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(LogPlayerPaused()); // NB: can only step while in paused state	
} 
 
//
// Handle the main user command to replay a log file.
// Handled in Mainframe instead of App because we manage the player UI.
// 
void CMainFrame::OnFileReplaylog() 
{
	CString strPathName, strFileName;
	CFileDialog dlg(TRUE, "log", /* filename: */ "*.log");

	// get file name
	if (dlg.DoModal() == IDOK) {
		strPathName = dlg.GetPathName();
		strFileName = dlg.GetFileName();
	}
	else 
		return;

	// ensure player control is created and visible
	if (m_pPlayDlg->GetSafeHwnd() == 0)
		OnViewPlaycontrol();

	// show file title in player control UI
	m_pPlayDlg->GetDlgItem(IDC_LOGFILE_NAME)->SetWindowText(strFileName);
	// also update the dlg's pathname (invisible) so play command can get it if play again. 
	m_pPlayDlg->m_strPathName = strPathName;

	// and ask the app to start the playback process
	theApp.StartPlaybackMode(strPathName, m_pPlayDlg);
}
    
///////////////////////////////////////////////////////////////////////////////////
// Application UI for giving some feedback during help system RPC's:
//
// Puts window into visible wait state during long DDE calls:
// Disables input and shows wait cursor during this state.

// Flag can be set to suppress normal clearing of hint on DDE calls in certain cases
BOOL CMainFrame::SetClearHintOnDde(BOOL bClear)
{
	BOOL bOld = m_bClearHintOnDde;
	m_bClearHintOnDde = bClear;
	return bOld;
}

void CMainFrame::BeginDdeWait(LPCTSTR pszMsg)
{
	// We try to ensure that only one synchronous help system call is possible
	// at a time (re-entrant calls to DDEML will fail) by disabling mainframe (which
	// disables all its children) during DDE waits to prevent receipt of any user 
	// interface events that could lead to re-entrant DDE calls. So if it happens, it 
	// means there's a bug. 
   	ASSERT(! m_bInDdeWait);	
	// Still we may have to deal with this error gracefully in release build. To 
	// handle errors leading to possible re-entrant calls, we increment a counter
	// on each BeginDdeWait and decrement on each EndDdeWait. The saved disabled state
	// m_bWasDisabled represents the state before the first one, and gets restored 
	// only after matching end bracket.
	++m_bInDdeWait;		//	inc count of nested calls

   	theApp.BeginWaitCursor();
   	BOOL bWasDisabled = EnableWindow (FALSE); 		
   	if (m_bInDdeWait == 1)
		m_bWasDisabled = bWasDisabled;	// save state before first DDE call
   	if (!pszMsg)
		pszMsg = "Waiting for response from Tutor";	// default
   	SetMessageText(pszMsg);	

	// Clear current hint on any DDE call (unless flag unset)
	if (m_bClearHintOnDde) {
		CHintView* pHint = (CHintView*) theApp.FindView(RUNTIME_CLASS(CHintView));
		if (pHint)
			pHint->ClearHint();
	}
}
    
BOOL CMainFrame::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
    // Continue to show hourglass cursor if called while inside DDE waits
 	// This is needed because DDEML pumps messages during the call.
    if (m_bInDdeWait) {
    	HCURSOR hCursor = AfxGetApp()->LoadStandardCursor(IDC_WAIT);
    	::SetCursor(hCursor);
    	return TRUE;
    }
    return CMDIFrameWnd::OnSetCursor(pWnd, nHitTest, message);
}
    
void CMainFrame::EndDdeWait()
{ 
    ASSERT(m_bInDdeWait);
	--m_bInDdeWait;				// dec count of nested calls
    if (m_bInDdeWait == 0 && ! m_bWasDisabled)
    	EnableWindow(TRUE);		// re-enable if it was enabled before 1st call
    theApp.EndWaitCursor();
    
	if (m_wndStatusBar )
    	SetMessageText(AFX_IDS_IDLEMESSAGE);	
}

// notification before async DDE request (no waiting, but still want to clear hint).
void CMainFrame::OnDdeSend()
{
	if (m_bClearHintOnDde) {
		CHintView* pHint = (CHintView*) theApp.FindView(RUNTIME_CLASS(CHintView));
		if (pHint)
			pHint->ClearHint();
	}

}

///////////////////////////////////////////////////////////////////////////////////////
// Help Commands implemented by frame because general, not specific to a view.
// Also hint/help message pane management helpers.
// Hint followup commands are implemented here even though state info required
// to enable them is in hint pane, so that they will be available from menu
// even if hint pane is not the active view.
//
void CMainFrame::OnGetProcHelp() 
{
	LogEventf(EV_HELP_HINT, "");	// log the command event for playback

	// This can require long wait for assessor update, so we set a
	// longer timeout for the helpsys call.
    // HelpIfcSetCallParms(5 * 60 * 1000); // 5 minutes better be long enough
	LPCTSTR pszResult = HelpSystemExecf( "(Get-Proc-Help)" );
	
	// Show the hint in the message pane. Routine handles failure return.
	// Successful reply will initiate tutor mode. This means we may wait until 
	// reply arrives before changing modes visibly.
    ShowHint(pszResult, Hint);
}

void CMainFrame::OnUpdateGetProcHelp(CCmdUI* pCmdUI) 
{
	// Hint command is only available inside a problem (quant or qual), with
	// procedural helper configured, running and hinting not explicitly suppressed 
	// for this problem by helpsys on open.
	// Also disabled while already *inside* a hint sequence in the hint pane
	CFBDDoc* pDoc = (CFBDDoc*) theApp.GetDocument();
	CHintView* pHintPane = theApp.GetHintView();
	CChatView* pChatPane = theApp.GetChatView();
	BOOL bInHintSequence = (pHintPane && pHintPane->m_bInHintSequence)
							|| (pChatPane && pChatPane->m_bInHintSequence);
	pCmdUI->Enable(pDoc != NULL && 
		(pDoc->m_nProblemType == PROB_QUANT || pDoc->m_nProblemType == PROB_QUAL) &&
		(theApp.m_wHelpFlags & fProcedural) &&
		HelpSystemIsConnected() &&
		!theApp.m_bNoHints &&
		!bInHintSequence && ! theApp.m_bTutorMode);	
}

// Worker routine to show given hint in hint pane
// Now we allow !show-hint or !show-lesson commands as hint specs as well for the
// convenience of the help system.
void CMainFrame::ShowHint(LPCTSTR pszHintSpec, HintType type /*=Msg*/)
{
	// If hint spec begins with !, dispatch it as a command to be executed. It is probably
	// !show-hint which will just come back to this routine after stripping the command..
	// But maybe in the in the future the help system will want to return a command to show
	// a mini-lesson in the middle of a hint sequence. Also, it is convenient for code on the
	// other side if it can always return a !show-hint in all contexts.
	if (pszHintSpec && pszHintSpec[0] == '!' && pszHintSpec[1] != '\0') {
		ExecuteHintCmd(&pszHintSpec[1]);
		return;
	}

	// Note Successful whatswrong result initiates tutor mode

	// Try to get the ChatView first.  If it exists, use it.
	CChatView* pCView = theApp.GetChatView();
	if (pCView) {
		pCView->AddSystemText(pszHintSpec, type);
		return;
	}
	
	// else look for Andes hint pane
	CHintView* pHint = theApp.GetHintView();
	if (pHint) {
		pHint->SetHintSpec(pszHintSpec, type); // ensures hint is visible
	}
}

// Dispatch a command received during a hint sequence. Needed to notify helpsystem of
// special results.
void CMainFrame::ExecuteHintCmd(LPCTSTR pszHintCmd)
{
	CString strCmd(pszHintCmd);
	
	// Just dispatch it through the command interpreter
	ExecuteCmd(pszHintCmd);
	
	// Wrap showing of mini-lesson in notification to helpsys
	CString strCmdName = strCmd.Left(strCmd.Find(' '));
	if (strCmdName.CompareNoCase("show-lesson") == 0) 
	{
		// send response to  tutor to tell us what to do next.
		ShowHint(HelpSystemExecf("(handle-student-response close-lesson)"));
	}
}

// For enabling/disabling hint followup commands.
// Helper tests whether followup button is currently enabled in the 
// appropriate pane, given followup letter code ('e', 'h', 'w')
BOOL CMainFrame::FollowupEnabled(int ch)
{
	CHintView* pHint;
	CChatView* pCView;
	return ((pHint=theApp.GetHintView()) && pHint->m_strBtns.Find(ch) != -1) 
					|| 
		   ((pCView=theApp.GetChatView()) && pCView->m_strBtns.Find(ch) != -1);
}

void CMainFrame::OnUpdateFollowupExplain(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(FollowupEnabled('e') && ! m_bInDdeWait);
}

void CMainFrame::OnUpdateFollowupHow(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(FollowupEnabled('h') && ! m_bInDdeWait);
}

void CMainFrame::OnUpdateFollowupWhy(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(FollowupEnabled('w') && ! m_bInDdeWait);
}

// Followup commands must disable the default automatic hint clearing on DDE calls to
// help system, since they are continuing the current hint sequence.
void CMainFrame::OnFollowupExplain() 
{
	LogEventf(EV_HELP_EXPLAIN, "");
	SetClearHintOnDde(FALSE);
	ShowHint( HelpSystemExecf("(Explain-More)"), Hint);
	SetClearHintOnDde(TRUE);
}

void CMainFrame::OnFollowupHow() 
{
	LogEventf(EV_HELP_HOW, "");
	SetClearHintOnDde(FALSE);
	ShowHint( HelpSystemExecf("(Hint-Next-Substep)"), Hint);
	SetClearHintOnDde(TRUE);	
}

void CMainFrame::OnFollowupWhy() 
{
	LogEventf(EV_HELP_WHY, "");
	SetClearHintOnDde(FALSE);
	ShowHint( HelpSystemExecf("(Why)"), Hint);
	SetClearHintOnDde(TRUE);
}

// Handle user followup command to dismiss message ("Hide" in ANDES, "OK" in chat)
void CMainFrame::OnFollowupHide()
{
	LogEventf(EV_HINT_HIDE, "");

	// for hint view, hide it. This will reset the hint sequence flag.
	CHintView* pHintView = theApp.GetHintView();
	if (pHintView)
		pHintView->HideHint();

	// for chat view, just means ends tutor mode and clear hint sequence flag.
	CChatView* pChatView = theApp.GetChatView();
	if (pChatView)
		pChatView->DismissMsgMode();

	// defensive programming, just to be safe:
	theApp.SetTutorMode(FALSE);	
}

void CMainFrame::OnUpdateFollowupHide(CCmdUI* pCmdUI) 
{
	// OK to just leave it always enabled, since not in UI unless in hint
	pCmdUI->Enable(TRUE);	
}

// worker routine for dispatching followup hyperlink clicks
// command is currently help system command name.
void CMainFrame::DoHelpRequest(LPCTSTR pszHelpCmd)
{
	// dispatch standard help/followup commands to handlers. 
	// Could use general command string dispatcher, but this is simpler. Also,
	// this way logged exactly the same as before we moved cmds into links. 
	if (strcmp(pszHelpCmd, "Hide") == 0) {
		SendMessage(WM_COMMAND, ID_FOLLOWUP_HIDE);
	} else if (strcmp(pszHelpCmd, "Explain-More") ==  0) {
		SendMessage(WM_COMMAND, ID_FOLLOWUP_EXPLAIN);
	} else if (strcmp(pszHelpCmd, "Hint-Next-Substep") == 0) {
		SendMessage(WM_COMMAND, ID_FOLLOWUP_HOW);
	} else if (strcmp(pszHelpCmd, "Why") == 0) {
		SendMessage(WM_COMMAND, ID_FOLLOWUP_WHY);
	} else {
		TRACE("Unknown help cmd for hyperlink: %s\n", pszHelpCmd);
		// Send it as help call anyhow. This allows extensibility by helpsys by
		// embedding custom callbacks in hints it sends.

		// this DDE not caused by hint-clearing student action
		SetClearHintOnDde(FALSE);
		// how log the initiating action (here as help-link click)
		LogEventf(EV_HELP_LINK, "%s", pszHelpCmd);
		// call helpsys func. result should be hint spec
		ShowHint(HelpSystemExecf("(%s)", pszHelpCmd), Hint);
		SetClearHintOnDde(TRUE);
	}
}
//
// Other help menu commands: equation review, textbook (experiment only) 
//
void CMainFrame::OnPhysReviewEqn() 
{
	// Goto Winhelp page.
	//AfxGetApp()->WinHelp((ID_PHYS_REVIEW_EQN + 0x00010000), HELP_CONTEXT);
	// Now show in our minilesson viewer.
	// theApp.ShowLesson("Principles.html");
	// now run the psm dialog, and let them use the Help command
	CChatView* pChatView = theApp.GetChatView();
	if (pChatView)
		pChatView->GetEquation(/*bSubmit*/ FALSE);
}

// Review physics equation enabled by same function:
void CMainFrame::OnUpdateTextbook(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(theApp.m_wHelpFlags & fTextbook);	
}

// Show the online HTML textbook browser.
// This is another instance of the BrowsDlg
void CMainFrame::OnTextbook() 
{
	// Log the user command
	LogEventf(EV_SHOW_TEXTBOOK, "");

	//
	// C++ object persists across uses. though Windows dlg isn't, to save last URL
	// and window position. If URL is not set from earlier use, init to contents page
	//
	static CString strTextbookUrl;	// saves contents page from reg or prompt
	if (m_dlgTextbook.m_strUrl.IsEmpty()) 
	{
		// Load textbook start page from registry as needed
		// entry should be full pathname
		if (strTextbookUrl.IsEmpty())
			strTextbookUrl = theApp.GetProfileString("Settings", "Textbook", "");

		// if not found in reg, prompt user for it
		if (strTextbookUrl.IsEmpty()) {
			CFileDialog dlgOpen(TRUE, NULL, NULL,  OFN_NOCHANGEDIR);
			dlgOpen.m_ofn.lpstrInitialDir = g_strAndesDir;
			if (dlgOpen.DoModal() != IDOK) 
    			return;
			strTextbookUrl = dlgOpen.GetPathName();
			// Remember in registry for next time
			theApp.WriteProfileString("Settings", "Textbook", strTextbookUrl);
		}

		// set browser to open there
		m_dlgTextbook.m_strUrl =  strTextbookUrl;
	}

	// Make sound as if deactivating app
	theApp.MakeSound(sndDeactivateApp);

	// run the browser dialog modally
	m_dlgTextbook.DoModal();

	// note close in log file, in case closed with 
	LogEventf(EV_BROWSER_CLOSE, "");

	// Make sound as if reactivating app
	theApp.MakeSound(sndActivateApp);
}

// open the Web support page in the user's browser
void CMainFrame::OnWebSupport() 
{
	// hardcoded default can be overridden by registry setting:
	CString strPage = theApp.GetProfileString("Settings", "WebSupport", 
					  "http://www.andes.pitt.edu/FAQ");

	// try to ShellExec the URL to show in default browser. No failure message.
	HINSTANCE hInst = ShellExecute(NULL, "open", strPage, NULL, NULL, SW_SHOWNORMAL); 
}

void CMainFrame::OnHelpEmail() 
{
	// try to ShellExec the URL to show in default browser. No failure message.
	HINSTANCE hInst = ShellExecute(NULL, "open", "mailto:andesits@pitt.edu", NULL, NULL, SW_SHOWNORMAL); 
}


// For evaluation Andes: collect a text comment into the log file.
void CMainFrame::OnComment() 
{
	// Run dialog to collect the comment text. 
	// CommentDlg knows how to make requisite log entries.
	CCommentDlg dlg;
	dlg.DoModal();
}

// Worker routine to show windows calculator. Public so useable from elsewhere.
void CMainFrame::BringUpCalculator()
{
	// Check if Windows calculator already running by searching for its window by title.
 	CWnd* pCalcWnd;
 	if (pCalcWnd = FindWindow(NULL, "Calculator"))
 	{
 		pCalcWnd->BringWindowToTop();
 		pCalcWnd->ShowWindow(SW_SHOWNORMAL);
 		pCalcWnd->UpdateWindow();
 	}
 	else // start it running from shell
 	{ 
 		HINSTANCE hInst = ShellExecute(NULL, "open", "calc", NULL, NULL, SW_SHOWNORMAL); 
 		if ((int) hInst <= 32) {
 			TRACE("Shellexec failed, code = %d\n", (int) hInst);
 		}
	}
}

void CMainFrame::OnCalculator() // Menu command handler (protected)
{
	BringUpCalculator();	
}

// 
// For customizing mainframe menu for student/author mode
//

// table of config-dependent commands only shown to authors
static const int AuthorCmdIDs[] = 
{
   	ID_AUTHORMODE,		// Main command to enter author mode (view menu)
   	// File menu commands:
   	ID_FILE_NEW,
   	ID_FILE_OPEN,		// untyped file open cmd only for authors, students use typed
  	ID_FILE_EDITEXAMPLE,
	ID_FILE_EDIT_PROBSET,
	ID_SHOWLESSON,		// Developer command for testing
   	ID_SHOWRULEDLG,		// Developer command for testing
   	ID_FILE_REPLAYLOG,	// Ditto
	ID_FILE_SHOWDEMO,	// And another
/* should be OK to allow to students now that multiple student opens are blocked
   	ID_FILE_MRU_FILE1,	// Only authors can open from MRU, students go via task dialog.
*/
   	// Edit commands
   	ID_OLE_INSERT_NEW,	// For OLE items
   	ID_OLE_VERB_FIRST,
	ID_ADD_HELP_FILE,
   	// View menu commands
   	ID_VIEW_OPTIONS,	// sets options for example view
   	ID_VIEW_FONT,		// lets author play with fonts
   	ID_VIEW_PLAYCONTROL,// shows the log player control
	ID_VIEW_PTR_DLG,	// shows/hides the demo pointer "agent"
	ID_VIEW_AUTHORBAR,	// shows/hides the author's drawing palette
	// Help menu commands
	ID_COMMENT,			// DEMO: disabled since no log upload
};
#define NAUTHORCMDS (sizeof(AuthorCmdIDs) / sizeof(AuthorCmdIDs[0]))
    
void CMainFrame::OnInitMenuPopup(CMenu* pPopupMenu, UINT nIndex, BOOL bSysMenu) 
{
	TRACE("in popup index is %d\n", nIndex);
   	// Our menu resources are defined to contain all possible commands. On menu
   	// popup we customize for current configuration by deleting inappropriate ones
	
	// !!! If this modifies the menu object used for the current window (?) there 
	// is no point doing more than once per problem open.
   
   	// Delete inapplicable help commands:
   	if ( ! (theApp.m_wHelpFlags & fProcedural))
   		pPopupMenu->DeleteMenu(ID_GET_PROC_HELP, MF_BYCOMMAND);
/*	leave WhatsWrong greyed, because separator after
	if ( ! (theApp.m_wHelpFlags & fWhatsWrong))
   		pPopupMenu->DeleteMenu(ID_HELP_WHATSWRONG, MF_BYCOMMAND);
*/
   
   	// Delete all author-only commands if "author privileges" not set in registry
   	if (! theApp.m_bAuthor) 
   	{
   		for (int i = 0; i < NAUTHORCMDS; i++) 
   			pPopupMenu->DeleteMenu(AuthorCmdIDs[i], MF_BYCOMMAND);
   	}


	if (nIndex == 4)//variablemenu
	{
		CString strPopup;
		pPopupMenu->GetMenuString(0, strPopup, MF_BYPOSITION);
		if (strPopup.Find("New Variable") > 0)
		{
			CVarMenu mnuVar;
			mnuVar.CreatePopupMenu();
			mnuVar.AttachProblemMenu(theApp.GetDocument()->m_wConcept);
			pPopupMenu->ModifyMenu(0, MF_BYPOSITION|MF_POPUP, (UINT)mnuVar.m_hMenu, "&Add New Variable");
		}
	}
   	// Base class will enable/disable commands now on menu
   	CMDIFrameWnd::OnInitMenuPopup(pPopupMenu, nIndex, bSysMenu);
}

#if 0
// code to delete the Principle menu if unused-- but where to put it?
	// remove principle menu if principle window not in use
	if (! GetProfileInt("Settings", "Principle Window", 0) ) {
		// can only delete by position, so must search to find it by name
		for (int i = 0; i < pMenu->GetMenuItemCount(); i++) {
			CString strItem;
			pMenu->GetMenuString(i, strItem, MF_BYPOSITION);
			if (strItem == "Principles") {
				pMenu->DeleteMenu(i, MF_BYPOSITION);
				break;
			}
		}
	}
#endif 

void CMainFrame::OnUpdateViewAuthorbar(CCmdUI* pCmdUI)	// cmd toggles author's palette
{
	pCmdUI->Enable(theApp.m_bAuthorMode);	// only enable when authoring
	OnUpdateControlBarMenu(pCmdUI);			// automatically handles check/uncheck
}

//
// Helpers for application palette management, originally added by the Palette Support
// component gallery item. Not fully used, but we must respond in some way to palette 
// messages to display imported bitmaps in the problem correctly.  
BOOL CMainFrame::OnQueryNewPalette()
{
#if 0 // not used until we add code to set a custom palette
    	// CG: This function was added by the Palette Support component
	if (m_pPalette == NULL)
		return FALSE;
    	
	// BLOCK
	{
		CClientDC dc(this);
		CPalette* pOldPalette = dc.SelectPalette(m_pPalette,
			GetCurrentMessage()->message == WM_PALETTECHANGED);
		UINT nChanged = dc.RealizePalette();
		dc.SelectPalette(pOldPalette, TRUE);

		if (nChanged == 0)
			return FALSE;
	}

	Invalidate();

	return TRUE;
#endif 0
	// For now, just repaint FBDView in case it contains embedded bitmaps. The OLE code
	// that renders bitmaps should attempt to get the colors into the system palette again
	if (AfxGetApp()->m_pMainWnd) // if not set, GetFBDView will fail (bug)
	{		
		CFBDView* pFBDView = theApp.GetFBDView();
		if (pFBDView)
			pFBDView->Invalidate();
	}
	return FALSE;		// we didn't actually realize any palette entries
}
    
void CMainFrame::OnPaletteChanged(CWnd* pFocusWnd)
{
	// CG: This function was added by the Palette Support component
	if (pFocusWnd == this || IsChild(pFocusWnd))
		return;
	
	OnQueryNewPalette();
}
    
CPalette* CMainFrame::SetPalette(CPalette* pPalette)
{
	// CG: This function was added by the Palette Support component

   	// Call this function when the palette changes.  It will
	// realize the palette in the foreground to cause the screen
	// to repaint correctly.  All calls to CDC::SelectPalette in
	// painting code should select palettes in the background.

	CPalette* pOldPalette = m_pPalette;
	m_pPalette = pPalette;
	OnQueryNewPalette();
	return pOldPalette;
}

//
// Managing border controls:
//

// Following moves frame in and out of a minimal frame configuration, 
// needed by example view to maximize display space for long examples.
void CMainFrame::HideBars()
{
   	ShowControlBar(&m_wndToolBar, FALSE, FALSE);
   	ShowControlBar(&m_wndStatusBar, FALSE, FALSE);
   	ShowControlBar(&m_wndEQBar, FALSE, FALSE);

}
    
void CMainFrame::ShowBars()
{
   	ShowControlBar(&m_wndToolBar, TRUE, FALSE);
   	ShowControlBar(&m_wndStatusBar, TRUE, FALSE);
   	ShowControlBar(&m_wndEQBar, TRUE, FALSE);
}
    
// 
// For window state persistence via profile section in registry. Code from Prosise Ch7.
// This code correctly persists iconized state, but starting up iconized is 
// problematic for this app, since login and task dialog must show anyway. This
// issue handled on restore.
// 
static const char *szWindowKey = "Main Window";	// section name
static const char	*szFlags = "Flags";
static const char	*szShowCmd = "ShowCmd";
static const char	*szLeft = "Left";
static const char	*szTop = "Top";
static const char	*szRight = "Right";
static const char	*szBottom = "Bottom"; 

void CMainFrame::SaveWindowState()
{
	CWinApp* pApp = AfxGetApp();
	
	WINDOWPLACEMENT wp;
    wp.length = sizeof (WINDOWPLACEMENT);
	GetWindowPlacement (&wp);
    
	pApp->WriteProfileInt (szWindowKey, szFlags, wp.flags);
    pApp->WriteProfileInt (szWindowKey, szShowCmd, wp.showCmd);
    pApp->WriteProfileInt (szWindowKey, szLeft, wp.rcNormalPosition.left);
    pApp->WriteProfileInt (szWindowKey, szTop, wp.rcNormalPosition.top);
    pApp->WriteProfileInt (szWindowKey, szRight, wp.rcNormalPosition.right);
    pApp->WriteProfileInt (szWindowKey, szBottom, wp.rcNormalPosition.bottom);
}
    
// restore previous state from registry, returning T if succeeded
BOOL CMainFrame::RestoreWindowState()
{
   	CWinApp* pApp = AfxGetApp();
   
   	WINDOWPLACEMENT wp;
    wp.length = sizeof (WINDOWPLACEMENT);
    GetWindowPlacement (&wp);
    
	if (((wp.flags =
         pApp->GetProfileInt (szWindowKey, szFlags, -1)) != -1) &&
    ((wp.showCmd =
         pApp->GetProfileInt (szWindowKey, szShowCmd, -1)) != -1) &&
    ((wp.rcNormalPosition.left =
         pApp->GetProfileInt (szWindowKey, szLeft, -1)) != -1) &&
    ((wp.rcNormalPosition.top =
         pApp->GetProfileInt (szWindowKey, szTop, -1)) != -1) &&
    ((wp.rcNormalPosition.right =
         pApp->GetProfileInt (szWindowKey, szRight, -1)) != -1) &&
    ((wp.rcNormalPosition.bottom =
         pApp->GetProfileInt (szWindowKey, szBottom, -1)) != -1)) 
	{
   		wp.rcNormalPosition.left = min (wp.rcNormalPosition.left,
            ::GetSystemMetrics (SM_CXSCREEN) -
               ::GetSystemMetrics (SM_CXICON));
        wp.rcNormalPosition.top = min (wp.rcNormalPosition.top,
            ::GetSystemMetrics (SM_CYSCREEN) -
            ::GetSystemMetrics (SM_CYICON));
   
		// Don't come up minimized, simulate restore from minimized state instead.
		if (wp.showCmd == SW_MINIMIZE || wp.showCmd == SW_SHOWMINIMIZED) {
			// Must maximize if it was maximized before iconized.
			if (wp.flags & WPF_RESTORETOMAXIMIZED)
				wp.showCmd = SW_SHOWMAXIMIZED;
			else
				wp.showCmd = SW_RESTORE;
		}

        SetWindowPlacement (&wp);
        return TRUE;
	}
    return FALSE;
    
}
    
//
// OnDDEExecute: We override the MFC default in CMainframe to allow processing DDE 
// execute commands from the help system even while mainframe is disabled (due to
// dialog box or because we are in the midst of our own DDE call).
// Code is copied from the MFC source.
 
// macro to determine number of elements in an array (not bytes)
#define _countof(array) (sizeof(array)/sizeof(array[0]))
    
// always ACK the execute command - even if we do nothing
LRESULT CMainFrame::OnDDEExecute(WPARAM wParam, LPARAM lParam)
{
    // unpack the DDE message
    UINT unused;
    HGLOBAL hData;
    VERIFY(UnpackDDElParam(WM_DDE_EXECUTE, lParam, &unused, (UINT*)&hData));
    
    // get the command string
    TCHAR szCommand[_MAX_PATH * 2];
    LPCTSTR lpsz = (LPCTSTR)GlobalLock(hData);
    lstrcpyn(szCommand, lpsz, _countof(szCommand));
    GlobalUnlock(hData);
    
    // acknowledge now - before attempting to execute
    ::PostMessage((HWND)wParam, WM_DDE_ACK, (WPARAM)m_hWnd,
    	ReuseDDElParam(lParam, WM_DDE_EXECUTE, WM_DDE_ACK,
    	(UINT)0x8000, (UINT)hData));

  /* This test removed:
   	// don't execute the command when the window is disabled
   	if (!IsWindowEnabled())
  	{
   		TRACE1("Warning: DDE command '%s' ignored because window is disabled.\n",
   			szCommand);
   		return 0;
   	}
  */

    // execute the command
    if (!AfxGetApp()->OnDDECommand(szCommand))
    	TRACE1("Error: failed to execute DDE command '%s'.\n", szCommand);
    
    return 0L;
}

//
// Demo mode support.
// This is a variant of log playback mode used to show demos.
//

// Developer menu command to choose a log file and run as demo, for testing
void CMainFrame::OnFileShowdemo() 
{
	CString strPathName, strFileName;
	CFileDialog dlg(TRUE, "log", /* filename: */ "*.log");
	 // Start the dialog in Demos directory.
	CString strDir = g_strAndesDir + g_szDemoDir;
	dlg.m_ofn.lpstrInitialDir = strDir;

	// get file name
	if (dlg.DoModal() != IDOK) 
		return;
	strPathName = dlg.GetPathName();
	
	ShowDemo(strPathName);
}

// ShowDemo: worker routine to start running a log file in demo mode.
// currently requires full pathname (to be changed to Andes-relative filename)
void CMainFrame::ShowDemo(LPCSTR pszPathName)
{
	// ensure demo player control is created and visible
	if (m_pDemoDlg->GetSafeHwnd() == 0) 
	{
		// ensure C++ obj is constructed
		if (! m_pDemoDlg)
			m_pDemoDlg = new CDemoDlg(this);

		// create Windows dialog, passing desktop as parent/owner.
		CWnd* pWndDesktop = CWnd::FromHandle(::GetDesktopWindow());
		m_pDemoDlg->Create(pWndDesktop); // created invisible in dialog template

		// place it at appropriate location on screen. For now
		// we put it along top of screen, with left midway across (indep of frame). 
		CClientDC dc(this);
		int xMidScreen = dc.GetDeviceCaps(HORZRES)/2;

		// make it a topmost window to ensure always visible
		m_pDemoDlg->SetWindowPos(&wndTopMost, xMidScreen, 0, /*ignored:*/ 0, 0, 
			                      SWP_NOSIZE | SWP_SHOWWINDOW);
	}
#if 0 // don't show ptr till demo script does it.
	// ensure Demo pointer "agent" is showing for pointing and pop-up messages
	// (!!! may come up at bad default position)
	if (m_pPtrDlg->GetSafeHwnd() == 0 || ! m_pPtrDlg->IsWindowVisible())
		OnViewPtrDlg();
#endif 0
	// flag that we are running in demo mode
	theApp.m_bDemoMode = TRUE;
	
	// and ask the app to start the playback process, with the Demo UI control
	theApp.StartPlaybackMode(pszPathName, m_pDemoDlg);
}

// return appropriate player control dialog (PlayDlg or DemoDlg). May be NULL if
// not in log playback mode.
CDialog* CMainFrame::GetPlayerControl() 
{ 
	if (theApp.m_bDemoMode)
		return m_pDemoDlg;
	else
		return m_pPlayDlg;
} 

// worker routine to cleanup and exit out of demo mode
void CMainFrame::ExitDemoMode()
{
	// Hide the pointer agent. 
	if (PtrIsVisible())
		OnViewPtrDlg();	// destroys window and deletes C++ object.

	// Destroy the demo player control.
	if (m_pDemoDlg->GetSafeHwnd()) {
		delete m_pDemoDlg;
		m_pDemoDlg = NULL;

		LogPlayerSetUI(NULL);	// to be safe
	}

	// If current open problem is still demo problem (demo might have been cancelled in
	// middle), close it to restore previous state.
	CFBDDoc* pDoc = (CFBDDoc*) theApp.GetDocument();
	if (pDoc && pDoc->m_bDemoProblem)
		pDoc->OnCloseDocument();

	// now we are out of demo mode
	theApp.m_bDemoMode = FALSE;

	// if there's no open problem, give students the task dialog again.
	if ((theApp.GetDocument() == NULL) && (! theApp.m_bAuthorMode)) {
		theApp.DoTaskSelect();
	}
}

// Variant of PLAYER_STOP command sent by demo-mode player
// If this is a demo, user's generating this command means to abort demo mode 
// in mid-playback (shouldn't be available in any other state). This is also
// sent as a result of closing the demo player, which happens automatically
// on normal demo playback completion.
void CMainFrame::OnPlayerQuitDemo() 
{
	LogPlayerStop();
	ExitDemoMode();	
}

// allocate CWnd and create Windows window as needed. Still invisible on create
void CMainFrame::EnsurePointerCreated()
{
	// enusre CWnd object and Windows window exist
	if (! m_pPtrDlg) 
		m_pPtrDlg = new CPtrWnd();

	if (m_pPtrDlg->GetSafeHwnd() == 0) {
		m_pPtrDlg->Create();
#if 0
		m_pPtrDlg->Create(CWnd::GetDesktopWindow());	// it is created visible
		// make it a top most window to ensure it's always visible, 
		m_pPtrDlg->SetWindowPos(&wndTopMost, /*ignored:*/0, 0, 0, 0, 
			                     SWP_NOMOVE | SWP_NOSIZE);
#endif 0
	}
}

BOOL CMainFrame::PtrIsVisible()	// safe if doesn't exist
{
	return (m_pPtrDlg->GetSafeHwnd() && m_pPtrDlg->IsWindowVisible());

}
void CMainFrame::HidePointer()	// hides but doesn't destroy -- position persists
{	
	if (m_pPtrDlg->GetSafeHwnd())
		m_pPtrDlg->ShowWindow(SW_HIDE); 
}

// Toggle showing/destroying demo pointer "agent". This is developer menu 
// command for testing, but also used internally to do work. Note that toggling off 
// with this method destroys the ptr agent and its state (may be bad).
void CMainFrame::OnViewPtrDlg() 
{
	if (PtrIsVisible()) {  // it exists and is visible, destroy it
		// Means position does not persist across uses of this
		// command to hide and show again. use HidePointer for alternative
		delete m_pPtrDlg;
		m_pPtrDlg = NULL;
		return;
	}
	// else not showing: ensure exists and make visible
	EnsurePointerCreated();
	m_pPtrDlg->ShowWindow(SW_SHOWNA);
}

// DemoMsg: handle command to show a popup msg in the course of a demo script:
// DemoMsg w/empty string means hide the message box.
void CMainFrame::DemoMsg(LPCTSTR pszMsg)
{
	// show it in the pointer msg balloon
	EnsurePointerCreated();		// should have happened on entry to demo mode
	ASSERT(m_pPtrDlg->GetSafeHwnd());
	m_pPtrDlg->ShowMsg(pszMsg);	
	// ensure pointer is showing? Else can have message balloon without ptr
}

// Public worker routine to actually move the pointer to some screen location. 
// Called by various containers' implementation's of PointToObject
void CMainFrame::MovePointerTo(int xScreen, int yScreen, CPtrWnd::PtrDir dir/*=UpLeft*/)
{
	// ensure pointer is showing (comes up at prev position if any)
	if (!PtrIsVisible())
		OnViewPtrDlg();
	
	TRACE("Mainframe: moving pointer to %d, %d\n", xScreen, yScreen);
	SetPointerDir(dir);
	m_pPtrDlg->MoveTo(xScreen, yScreen);
}

void CMainFrame::SetPointerDir(CPtrWnd::PtrDir dir)
{
	if (m_pPtrDlg->GetSafeHwnd()) m_pPtrDlg->SetDir(dir);
}
//
// Following supports log/script commands to move the pointer to a toolbar object
//

typedef struct {				// lookup table entry
	char*	szName;
	int		nID;
} CmdInfo;
static const CmdInfo cmdTbl [] =
{
"System",	ID_SYSTEM,
"Axes",		ID_DIAGRAM_COORDINATES,
"Force",	ID_DRAWVECTOR_FORCE,
"Velocity",	ID_DRAWVECTOR_VELOCITY,
"Acceleration",	ID_DRAWVECTOR_ACCELERATION,
"Displacement", ID_DRAWVECTOR_DISPLACEMENT,
"Torque", ID_DRAWVECTOR_TORQUE,
"Position", ID_DRAWVECTOR_RELPOS,
"Component",	ID_DRAWVECTOR_COMPONENT,
"E-Field", ID_DRAWVECTOR_EFIELD,
"B-Field", ID_DRAWVECTOR_BFIELD,
"Angle",	ID_ANGLE,
"Radius",	ID_LABEL_RADIUS,
"MotionDiagram",	ID_RULER,
"MotionBody",	ID_MOTION_BODY,
"Curve",	ID_POLYBEZIER,
"Guideline",	ID_GUIDE_LINE,
"Select",	ID_SELECT,
"2DMotion",	ID_2DMOTION,
"Open",		ID_FILE_OPEN,
"Save",		ID_FILE_SAVE,
"Close",	ID_FILE_CLOSE,
"Print",	ID_FILE_PRINT,
"Cut",		ID_EDIT_CUT,
"Copy",		ID_EDIT_COPY,
"Paste",	ID_EDIT_PASTE,    
"Helpmode",		ID_CONTEXT_HELP,
"Hint",		ID_GET_PROC_HELP,
"WhatsWrong", ID_HELP_WHATSWRONG,
"Calculate", ID_CALCULATE,
"GoGreek", ID_GREEK,
};
#define NCMDS (_countof(cmdTbl))

static int CmdNameToID(const char* name)	// fetch cmd id from table for named command
{
	for (register int i = 0;  i < NCMDS; ++i) 
		if (_stricmp(name, cmdTbl[i].szName) == 0) // case-insensitive to allow typos in script */
			return cmdTbl[i].nID;
	return -1;
}

// currently, only allow toolbar command name
void CMainFrame::PointToObject(LPCSTR pszObjID)
{
	// Map name to command id
	int nID = CmdNameToID(pszObjID);
	if (nID == -1) {
		TRACE("toolbar PointTo: unknown command %s\n", pszObjID);
		return;
	}

	// search for command on childframe's bar, then mainframe's.
	CToolBar* pBar = NULL;		// points to bar on which found
	int nIndex = -1;			// index of command button if found
	
	CChildFrame* pChildFrm = (CChildFrame*) MDIGetActive();
	if (pChildFrm &&
		(nIndex = pChildFrm->m_wndProblemBar.CommandToIndex(nID)) != -1 )
		pBar = &pChildFrm->m_wndProblemBar;
	else if ( (nIndex = m_wndToolBar.CommandToIndex(nID)) != -1)
		pBar = &m_wndToolBar;
	else if ( (nIndex = m_wndEQBar.CommandToIndex(nID)) != -1)
		pBar = &m_wndEQBar;
	else {
		TRACE("toolbar PointTo: item not found %s\n", pszObjID);
		return;
	}
	
	// place pointer at bottom right of item rect
	ASSERT(nIndex != -1);
	ASSERT(pBar != NULL);
	CRect rcItem, rcBar;
	pBar->GetWindowRect(rcBar);
	pBar->GetItemRect(nIndex, rcItem);	// in pixels relative to toolbar
	rcItem += rcBar.TopLeft();			// offsets rect to get screen coords
	MovePointerTo(rcItem.right, rcItem.bottom);
}


void CMainFrame::OnDropSolveFor(NMHDR * pNMHDR, LRESULT * pResult)
{
	CRect rcBtn, rcBar;
	int nIndex = m_wndEQBar.CommandToIndex(ID_SOLVEFOR);
	m_wndEQBar.GetItemRect(nIndex, rcBtn);	// in pixels relative to toolbar
	m_wndEQBar.ClientToScreen(&rcBtn);

	// Show popup menu w/variables below button
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_SOLVEFOR));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	// Subclass it for Greek letter drawing
	CGreekMenu mnuGreek;
	if (pPopup)
		mnuGreek.Attach(pPopup->GetSafeHmenu());

	// Hack to include a title pseudo-item -- make first item default so bold, 
	// but inactive since no handler for it.
	::SetMenuDefaultItem(pPopup->m_hMenu, 0, TRUE);

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_LEFTBUTTON, rcBtn.left, rcBtn.bottom,
		this);

	*pResult = 0;
}


void CMainFrame::PositionGreekMenu()
{
	CRect btnPos, mnuPos, frmRect;
	int nIndex = m_wndEQBar.CommandToIndex(ID_GREEK);
	m_wndEQBar.GetItemRect(nIndex, &btnPos);
	m_wndEQBar.ClientToScreen(&btnPos);
	
	m_dlgSymbolMenu.GetWindowRect(&mnuPos);
	int left = btnPos.left;
	int top = btnPos.bottom;
	
	// don't overhang right or below doc frame
	CFrameWnd* pWnd = MDIGetActive();
	if (pWnd == NULL) pWnd = this;
	pWnd->GetWindowRect(&frmRect);
	if (frmRect.right < (btnPos.left + mnuPos.Width()))
		left = btnPos.right -  mnuPos.Width();
	if (frmRect.bottom < (btnPos.bottom + mnuPos.Height()))
		top = btnPos.bottom - mnuPos.Height() - btnPos.Height();

	m_dlgSymbolMenu.MoveWindow(left, top, mnuPos.Width(), mnuPos.Height(), TRUE);
}

void CMainFrame::OnGreek() 
{
	if (!m_dlgSymbolMenu.IsWindowVisible()){
		PositionGreekMenu();
		// if (m_nLastActive == -1)
		//	m_pSymbolMenu->EnableButtons(FALSE);
		// else if (m_Edit[m_nLastActive].IsKindOf(RUNTIME_CLASS(CRichEditCtrl)))
		//	m_pSymbolMenu->EnableButtons(TRUE);

		// dialog may try to grab focus on showing (?) so bounce it back
		HWND hwndFocus = ::GetFocus();
		m_dlgSymbolMenu.ShowWindow(SW_SHOW);
		if (hwndFocus) ::SetFocus(hwndFocus);

		//if (m_nLastActive != -1)
		//	m_Edit[m_nLastActive].SetFocus();//set focus back to edit ctrl
	}										//so user doesn't get lost
	else{
		m_dlgSymbolMenu.ShowWindow(SW_HIDE);
		//if (m_nLastActive != -1)
		//	m_Edit[m_nLastActive].SetFocus();//set focus back to edit ctrl
	}								//so
	
}

void CMainFrame::OnUpdateGreek(CCmdUI* pCmdUI) 
{
	// If not showing, active view must enable to permit it to be shown
	// If showing, must be enabled so can take it down.
	pCmdUI->Enable(GreekMenuIsVisible());
}




// Place dialog (or any other) window over the equation pane at lower-right.
// This duplicates a function in CDrawObjDlg so it can be used for other windows.
// !!! CDrawObjDlgs should be changed to ask the mainframe to place them via this routine.
// Don't use this function if no equation pane.
//
// Attempts to place top-left corner of dialog over top-left corner of eqview, 
// adjusting dialog's top-left corner upward and/or leftward from there if necessary  
// to prevent dialog extending beyond right/bottom edges of the mainframe.
//
// !!! Bug: This can sometimes move top off screen above top of mainframe, for big dialogs on
// low-res (e.g. laptop) screens. Should check for this.
// Might also be more appropriate to adjust only on overflow of visible desktop workspace area,
// rather than mainframe border. (Mainframe is typically fullscreen, but users can reduce it.)

void CMainFrame::MoveDlgToBtmRight(CWnd *pDlg)
{
	int left, top;
	CRect dlgRect, frmRect;
	CRect eqRect(0, 0, 0, 0);
		
	pDlg->GetWindowRect(&dlgRect);
	CFrameWnd* pWnd = pDlg->GetParentFrame();
	pWnd->GetWindowRect(&frmRect);
	CWnd* pView = (CWnd*) theApp.GetEQView();
	if (pView != NULL)
		pView->GetWindowRect(&eqRect);
	else if (theApp.GetFBDView() == NULL) {
		// if no fbd view either, just center the window,
		// no reason to hide on bottom right
		pDlg->CenterWindow();
		return;
	}
	int dlgWidth = dlgRect.right-dlgRect.left;
	int dlgHeight = dlgRect.bottom-dlgRect.top;
	int eqWidth = eqRect.right - eqRect.left;
	int eqHeight = eqRect.bottom - eqRect.top;
	if (eqWidth >= dlgWidth)
		left = eqRect.left;
	else
		left = frmRect.right-dlgWidth;
	if (eqHeight >= dlgHeight)
		top = eqRect.top;
	else
		top = frmRect.bottom - dlgHeight-25;
	pDlg->MoveWindow(left, top, dlgWidth, dlgHeight);
}








