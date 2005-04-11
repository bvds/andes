// MainFrm.h : interface of the CMainFrame class
// 
// $Id: Mainfrm.h,v 1.2 2005/04/11 18:53:54 anders Exp $
/////////////////////////////////////////////////////////////////////////////
#ifndef MAINFRAME_INCLUDED
#define MAINFRAME_INCLUDED 1

#include "history.h"	// required for type IEventHandler.
#include "BrowsDlg.h"	// for CBrowserDlg
#include "PtrDlg.h"		// for CPtrDlg::PtrDir
#include "SymbolMenu.h"	// for CSymbolMenu

class CPlayDlg;			// forward declaration for pointer classes
class CDemoDlg;	
class CPtrWnd;
/* class CHintDlg; */

// Type of messages shown in hint window we control:
enum HintType 
{ 
	Hint,				// Help request response 
	WhatsWrong,			// Whatswrong response
	Msg,				// "pop-up" tutor message, unsolicited or on entry submission
};

class CMainFrame : public CMDIFrameWnd, public IEventHandler
{
   	DECLARE_DYNAMIC(CMainFrame)
public:
    CMainFrame();
    
// Attributes
public:
	BOOL m_bClosing;		// flag set on shutdown (suppresses task dlg after problem close)
    
// Operations
public:
	// toolbars etc:
	void ShowAuthorBar(BOOL bShow = TRUE);
	void ShowBars();
	void HideBars();
	void SetAngleText(LPCTSTR str);// status bar pane manipulation:
	void BringUpCalculator();
	// hint window:
	void ShowHint(LPCTSTR pszHintSpec, HintType type = Msg);
	
	// Demo mode command: 
	void ShowDemo(LPCSTR pszFileName);
	//		managing demo mode pointer ("agent"):
	void MovePointerTo(int xScreen, int yScreen, CPtrWnd::PtrDir dir = CPtrWnd::UpLeft);
	void SetPointerDir(CPtrWnd::PtrDir dir);
	void DemoMsg(LPCTSTR pszMsg);
	void HidePointer() ;
	BOOL PtrIsVisible() ;

	// fetch log playback control dialog (either demo or play dialog)
	CDialog* GetPlayerControl(); 
	
	// for persistent frame settings:
	void SaveWindowState();
	BOOL RestoreWindowState();

	// for managing UI to enter visible wait state during DDE calls
	void BeginDdeWait(LPCTSTR pszMsg = NULL);
	void EndDdeWait();
	BOOL InDdeWait() { return m_bInDdeWait; };
	void OnDdeSend();		// notification before async calls (no waiting).
	BOOL SetClearHintOnDde(BOOL bClear); // returns old val for save/restore

	// To dispatch followup commands from hyperlink clicks:
	void DoHelpRequest(LPCTSTR pszHelpFunc);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainFrame)
	public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
public:	
	virtual ~CMainFrame();
#ifdef _DEBUG
   	virtual void AssertValid() const;
   	virtual void Dump(CDumpContext& dc) const;
#endif

	CToolBar    m_wndEQBar;			// tools for equation view (calculate and greek)
protected:
	// control bar embedded members
	CStatusBar  m_wndStatusBar;		// standard status bar
	CToolBar    m_wndToolBar;		// problem tools available to students
	CToolBar	m_wndAuthorBar;		// drawing tools used by authors
	void DockControlBarLeftOf(CToolBar* Bar,CToolBar* LeftOf); // docks side by side

	// optional modeless floating tools:
	CPlayDlg*   m_pPlayDlg;			// modeless dialog for playback control
	CDemoDlg*	m_pDemoDlg;			// modeless dialog for demo play control
	CPtrWnd*	m_pPtrDlg;			// pointer for demos as modeless dialog.
/*	CHintDlg*	m_pHintWnd;			// popup hint window */
	CBrowserDlg   m_dlgTextbook;	// html textbook viewer
	CSymbolMenu m_dlgSymbolMenu;	// modeless dlg with menu of greek symbols
public:
	BOOL GreekMenuIsVisible() { return m_dlgSymbolMenu.IsWindowVisible(); }
	void HideGreekMenu() { m_dlgSymbolMenu.ShowWindow(SW_HIDE); }

protected:
	// status bar clock:
	int m_nTimePaneNo;				// remembers index of status bar clock pane
	afx_msg void OnUpdateTime(CCmdUI* pCmdUI);		// handles clock pane updating
	// status bar score
	int m_nScorePaneNo;				// remembers index of status bar score pane
	afx_msg void OnUpdateScore(CCmdUI* pCmdUI);		// handles score pane updating

	// RPC wait state management:
public://now public so can be accessed in dialogs
	static CWnd* FindFBDWindow();
	void ExecuteHintCmd(LPCTSTR pszHintCmd);
	void MoveDlgToBtmRight(CWnd* pDlg);
	BOOL FollowupEnabled(int ch);
	BOOL m_bInDdeWait;				// flag to show wait cursor inside long DDE calls
protected:
	BOOL m_bWasDisabled;			// saves prev enabled state across DDE waits
	BOOL m_bClearHintOnDde;			// whether help sys DDE call should clear hint window

	// Demo mode implementation:
	void EnsurePointerCreated();
	void ExitDemoMode();
	
	// palette management services
   	afx_msg BOOL OnQueryNewPalette();
   	afx_msg void OnPaletteChanged(CWnd* pFocusWnd);
   	CPalette* SetPalette(CPalette* pPalette);
   	CPalette* m_pPalette;			// custom palette for the app

	// Script/log/demo support
	// virtual BOOL DispatchEvent(EventID id, LPCSTR pszArgs);
	virtual void PointToObject(LPCSTR pszObjID);
    	
// Generated message map functions
protected:
	void PositionGreekMenu();
   	//{{AFX_MSG(CMainFrame)
   	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
   	afx_msg void OnViewPlaycontrol();
   	afx_msg void OnUpdateViewPlaycontrol(CCmdUI* pCmdUI);
   	afx_msg void OnTimer(UINT nIDEvent);
   	afx_msg void OnEnterIdle(UINT nWhy, CWnd* pWho);
   	afx_msg void OnPlayerPlay();
   	afx_msg void OnPlayerStop();
   	afx_msg void OnPlayerFastforward();
   	afx_msg void OnPlayerSkip();
   	afx_msg void OnPlayerPause();
   	afx_msg void OnUpdatePlayerPlay(CCmdUI* pCmdUI);
   	afx_msg void OnUpdatePlayerPause(CCmdUI* pCmdUI);
   	afx_msg void OnUpdatePlayerFastforward(CCmdUI* pCmdUI);
   	afx_msg void OnUpdatePlayerStop(CCmdUI* pCmdUI);
   	afx_msg void OnFileReplaylog();
   	afx_msg BOOL OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
   	afx_msg void OnCalculator();
   	afx_msg void OnGetProcHelp();
   	afx_msg void OnClose();
   	afx_msg void OnInitMenuPopup(CMenu* pPopupMenu, UINT nIndex, BOOL bSysMenu);
   	afx_msg void OnPhysReviewEqn();
	afx_msg void OnFileShowdemo();
	afx_msg void OnViewPtrDlg();
	afx_msg void OnUpdateFollowupExplain(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFollowupHow(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFollowupWhy(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFollowupHide(CCmdUI* pCmdUI);
	afx_msg void OnPlayerReplayHelp();
	afx_msg void OnUpdatePlayerReplayHelp(CCmdUI* pCmdUI);
	afx_msg void OnPlayerQuitDemo();
	afx_msg void OnActivateApp(BOOL bActive, HTASK hTask);
	afx_msg void OnUpdatePlayerGoto(CCmdUI* pCmdUI);
	afx_msg void OnUpdateViewAuthorbar(CCmdUI* pCmdUI);
	afx_msg void OnTextbook();
	afx_msg void OnUpdateGetProcHelp(CCmdUI* pCmdUI);
	afx_msg void OnComment();
	afx_msg void OnPlayerStep();
	afx_msg void OnUpdatePlayerStep(CCmdUI* pCmdUI);
	afx_msg void OnGreek();
	afx_msg void OnUpdateGreek(CCmdUI* pCmdUI);
	afx_msg void OnPlayerSnapshot();
	afx_msg void OnUpdatePlayerSnapshot(CCmdUI* pCmdUI);
	afx_msg void OnFollowupExplain();
	afx_msg void OnFollowupHow();
	afx_msg void OnFollowupWhy();
	afx_msg void OnFollowupHide();
	afx_msg void OnWebSupport();
	afx_msg void OnUpdateTextbook(CCmdUI* pCmdUI);
	afx_msg void OnHelpEmail();
	//}}AFX_MSG
	afx_msg LRESULT OnDDEExecute(WPARAM wParam, LPARAM lParam);
	afx_msg LONG OnClosePlayDlg(UINT wParam, LONG lParam);	
	afx_msg LONG OnCloseDemoDlg(UINT wParam, LONG lParam);
	afx_msg void OnDropSolveFor(NMHDR * pNMHDR, LRESULT *pResult);
  	DECLARE_MESSAGE_MAP()

private:
   	BOOL InitStatusBar(UINT *pIndicators, int nSize, int nSeconds);
};

// Application-specific windows messages we use
#define WM_CLOSE_PLAY_DLG		WM_APP + 10
#define WM_CLOSE_DEMO_DLG		WM_APP + 11
// Complete msg available on TCP event socket
#define WM_EVENT_MSG			WM_APP + 100

// 
// Note: Child window IDs used for additional toolbars in mainframe 
// and child (problem) frame are defined in MFC's standard control
// bar range, see CMainFrame::OnCreate.  We also use the same ID 
// values for the View menu commands, see Prosise p 720.
// The actual values have been hard-coded into resource.h. 
// (AFX_IDW_TOOLBAR == OxE800).
//
// #define IDW_DRAWINGBAR			(AFX_IDW_TOOLBAR + 5)
// #define ID_VIEW_DRAWINGBAR		IDW_DRAWING_BAR
// #define IDW_PROBLEMBAR			(AFX_IDW_TOOLBAR + 6)
// #define ID_VIEW_PROBLEMBAR		IDW_PROBLEM_BAR
// #define IDW_EQBAR				(AFX_IDW_TOOLBAR + 7)
// #define ID_VIEW_EQBAR			IDW_EQ_BAR

#endif // ! MAINFRAME_INCLUDED
/////////////////////////////////////////////////////////////////////////////
