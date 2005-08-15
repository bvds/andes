// FBD.h : main header file for the FBD application
// 
// $Id: Fbd.h,v 1.3 2005/08/15 04:28:13 anders Exp $

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CFBDApp: the one and only application object
// See FBD.cpp for the implementation of this class
//
#include "BrowsDlg.h"	// for CBrowserDlg

// Forward declarations:
class CFBDDoc;
class CFBDView;
class CEXView;
class CEQView;
class CPlanView;
class CVarView;
class CHintView;
class CChatView;
class CHiLevelVw;
class CTabView;
class CPrincView;
class CChildFrame;
class CEXPlanVw;
class CMainFrame;
class IPlayerUI;
class CFBDDocTemplate;
class CProblemSet;
class CProbSetView;

// Help option bit flags.  Configured from registry for experimental conditions
enum HelpFlag 
{
	fProcedural = 0x01,	// Enables "I'm stuck" hint requests (ID_GET_PROC_HELP)
	fConceptual = 0x02,	// Enables "Why button" on hints, minilessons
	fExample	= 0x04,	// Enables self-explanation features for example viewing
	fWhatsWrong = 0x08,  // Enables whats wrong help on errors
	fTextbook   = 0x10, // Equation review pages and (experiment) textbook

	// shorthand combinations:
	// Set wAllHelp for max help:
	wAllHelp = (fProcedural | fConceptual | fExample | fWhatsWrong | fTextbook), 
	wNoHelp   = 0x0000,	// Bitset for min help commands (though allows feedback)
	wFlagMode = fTextbook,
};

// Events optionally signalled with distinctive sounds to aid experiment recording
enum EventSound
{
	sndCorrectEntry,
	sndErrorEntry,
	sndHintMsg,
	sndWhatsWrongMsg,
	sndActivateApp,
	sndDeactivateApp,
	
	sndNUMSOUNDS,		// must be last, marks limit for sound-indexed iteration
};

class CFBDApp : public CWinApp
{
public:
	CFBDApp();
	//
	// Global status flags for modes affecting the whole application:
	//
	// Some of these might be changed to per-document or per-view basis, but
	// currently operating on the idea that only one doc is open at a time.
	//
	BOOL m_bAuthorMode;			// enable author-only commands to create/edit problems
	BOOL m_bTrainMode;			// in training card mode
	BOOL m_bDemoMode;			// running a demo file
	BOOL m_bTutorMode;			// in helpsys-controlled tutorial dialog
	void SetTutorMode(BOOL bTutorMode);
	
	// parameters configurable via profile entries in registry:
	BOOL m_bAuthor;				// run w/Author privileges (can enter AuthorMode)
	BOOL m_bPlaySounds;			// give audible feedback for select events
	BOOL m_bUsePrincipleWnd;	// Show Principle Window
	 
	// Help and feedback state:
	WORD m_wHelpFlags;			// Bitset: currently available help (may change)
	WORD m_wDefaultHelpFlags;   // Bitset, help configured at startup registry cmd line
	BOOL m_bNoHints;			// Prochelp can't give hints for current problem.
	// Feedback options:		!!! User option flags not currently used
	BOOL m_bFeedback;			// General: whether feedback currently on or off
	BOOL m_bCheckEquations;		// user option: whether to check equations
	BOOL m_bCheckDiagram;		// user option: whether to check diagram entries
	//  general feedback flag trumps user options:
	BOOL CheckEquations() { return m_bFeedback && m_bCheckEquations; };
	BOOL CheckDiagram()   { return m_bFeedback && m_bCheckDiagram; };
	int m_nFeedback;			// user option: feedback mode:
#define FEEDBACK_WAIT  0		//		Blocking (synchronous) mode
#define FEEDBACK_ASYNC 1		//		Non-blocking (asynchronous) mode (EQs only)
	// NB values must match radio button order in HelpOpts dialog
	
	// Example view options: mainly for our testing.
	// Stored in app, not view, so can change them between examples.
	int m_nMaskMode;			// how to display hidden text
#define MM_MASK			0		//	cover it with a grey mask
#define MM_LIGHTEN		1		//	draw it in very light grey
	// NB values must match radio button order in ViewOpts dialog
	int m_nGreyLevel;			// value for lighten mode betw 0 && 255
	enum { EXViewGrey = 180 };	// default grey level, changeable by user

	BOOL m_bNoHelp;				// cmd line flag to run without help system.
	CString m_strUserName;		// Student's handle entered in login window

	BOOL CanOpenProblem();		// test whether admissable to open a problem file
	BOOL CanOpenProblemSet();	// ditto for problem set
	BOOL m_bShellPrintOnly;		// flag that file opened for shell printing only.
	BOOL m_bRegisterOnly;		// flag that running to register filetypes only

	CString m_strAndesVersion;	// current Andes version as defined in version resource

	// 
	// Utilities to find principal objects from anywhere in the code:
	//
	CMainFrame* GetMainFrame() { return (CMainFrame*) AfxGetMainWnd(); }
	CDocument* GetActiveDoc();
	CView*     GetCurrentView(CRuntimeClass* pClass);
	CView*     FindView(CRuntimeClass* pClass); 
	CFBDDoc*   GetCurrentProblem(); // gets active problem; ignores problem sets
	// for backwards compatibility: (previously only one kind of doc could be current).
	CFBDDoc*   GetDocument() { return GetCurrentProblem(); }	
	CEXView*   GetEXView();
	CFBDView*  GetFBDView();
	CEQView*   GetEQView();
	CPlanView* GetPlanView();
	CEXPlanVw* GetEXPlanVw();
	CVarView*  GetVarView();
	CHintView* GetHintView();
	CChatView* GetChatView();
	CTabView*  GetTabView();
	CHiLevelVw*  GetHiLevelVw();
	CPrincView* GetPrincView();
	CChildFrame* GetChildFrame();
	CProblemSet* GetProblemSet();
	CProbSetView* GetProbSetView();


	// Helpers for communicating with training card help:

	//  Send msg from window to training card instance of winhelp:
	void CallTCardHelp(CWnd* pWnd, DWORD dwID, UINT nCmd = HELP_CONTEXT);
	//  Same as above, but sends only if currently in training mode:
	void SendTrainer(CWnd* pWnd, DWORD dwID, UINT nCmd = HELP_CONTEXT)
		{ if (m_bTrainMode) CallTCardHelp(pWnd, dwID, nCmd); }

	// For managing glossary of physics defs used for hint popups.
	BOOL LookupDef(LPCTSTR pszKey, CString& strDef); // loads glossary as needed
protected:
	CMapStringToString m_mapPhysicsDefs;			// glossary in memory
	void LoadGlossary();							// worker to load from text file

// Operations
public:
	void StartPlaybackMode(LPCTSTR pszPathName, 
						   IPlayerUI* pUI = NULL);	// begin playing back log file
	BOOL OnHelpSysCommand(LPCSTR pszCmd);			// dispatch cmd from helpsys
	void ShowLesson(LPCTSTR pszName = NULL);		// show a minilesson in the browser
	void DoShowLesson(LPCTSTR pszName);				// worker routine for above
	void OpenBrowser(LPCTSTR pszName);				// show page modelessly in the browser
	void CloseBrowser();							// close modeless browser if open
	void ShowVideoPage(CString strName);					// show the page for a video
	void ShowRuleQuery(LPCTSTR pszFileName);		// show a rule dialog.
	void ShowHintDdeCmd(LPCTSTR pszHintSpec);		// show a hint w/followup buttons
	int  DoWarningMessage(CString msg, CWnd* pParent = AfxGetMainWnd(), UINT nType = MB_OK);
	void DoInstructMessage(CString msg);
	int  DoInfoMessage(CString msg, UINT nType = MB_OK);
	void MakeSound(EventSound sound);
	void ShowTCard(LPCTSTR pszName);
	void DoTaskSelect();
	BOOL QueryUpload();						// prompt to upload experiment data

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CFBDApp)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	virtual BOOL OnDDECommand(LPTSTR lpszCommand);
	virtual BOOL SaveAllModified();
	virtual CDocument* OpenDocumentFile(LPCTSTR lpszFileName);
	virtual int DoMessageBox(LPCTSTR lpszPrompt, UINT nType, UINT nIDPrompt);
	//}}AFX_VIRTUAL

// Implementation
	// Saved ptrs to DocTemplates registered with MFC for our file types:
	CFBDDocTemplate* m_ptmplProblem;		// Problem file
	CFBDDocTemplate* m_ptmplExample;		// Example file, student view
	CFBDDocTemplate* m_ptmplExEdit;			// Example file with edit view
	CMultiDocTemplate* m_ptmplProbSet;		// Problem Set, student (work) mode
	CMultiDocTemplate* m_ptmplProbSetEdit;

protected:
	void LoadAndesSettings();		// loads app's stored values from registry.
	void InitVersion();			// Initialize Product version settings
	BOOL m_bUserInit;			// set => have completed user initialization
	BOOL m_bHelpSysInit;		// set => have completed helpsys initialization
	BOOL EnsureUserInit();		// Ensure completed init for interactive use

public:
	void DoInitialTaskSelect();
	CString m_strSessionId;
	BOOL m_bTerminated;			// flag that final cleanup has been performed.
	void DoFinalCleanup();

	BOOL IsRovingStudent();
	void TransferStudentFiles(BOOL bUpload);
	
	CString m_strRemoteHost;	// source if invoked to run as remote viewer (spy).
	BOOL IsRemoteViewer()		// true if running as remote viewer
	{ return ! m_strRemoteHost.IsEmpty(); };
	BOOL IgnoreInput()			// two cases in which we ignore user input
	{ return LogPlayerInPlayback() || IsRemoteViewer(); };

	BOOL EnsureHelpSysInit();	// Ensure help sys connected and initialized 
protected:
	int GetTCardID(CString name);

	// Lesson-viewer dialog object persists across modal uses to save size. pos
	// Probably should be owned by frame, but lesson view cmds are in app.
	CBrowserDlg   m_dlgLesson;	// mini-lesson viewer dlg
	
	//{{AFX_MSG(CFBDApp)
	afx_msg void OnAppAbout();
	afx_msg void OnAuthormode();
	afx_msg void OnUpdateAuthormode(CCmdUI* pCmdUI);
	afx_msg void OnHelpOptions();
	afx_msg void OnUpdateFileNew(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileOpen(CCmdUI* pCmdUI);
	afx_msg void OnViewOptions();
	afx_msg void OnFileEditexample();
	afx_msg void OnUpdateFileEditexample(CCmdUI* pCmdUI);
	afx_msg void OnShowlesson();
	afx_msg void OnFileOpenproblem();
	afx_msg void OnFileViewexample();
	afx_msg void OnFileOpen();
	afx_msg void OnShowruledlg();
	afx_msg void OnFileOpensolution();
	afx_msg void OnFileOpenproblemset();
	afx_msg void OnUpdateFileOpenproblemset(CCmdUI* pCmdUI);
	afx_msg void OnFileEditProbset();
	afx_msg void OnUpdateFileEditProbset(CCmdUI* pCmdUI);
	afx_msg void OnHelpVideos();
	afx_msg void OnHelpUnits();
	afx_msg void OnHelpConstants();
	//}}AFX_MSG
	// Handle custom msg signaling cmd input over TCP/IP cmd port:
	afx_msg LONG OnEventMsg(UINT wParam, LONG lParam);	
	DECLARE_MESSAGE_MAP()

	virtual BOOL PreTranslateMessage(MSG* pMsg);
};

//
// We make the one and only app object public so all code can access 
// global flags concisely (without needing AfxGetApp() + downcast):
//
extern CFBDApp theApp;
// 
// Global path info: Root directory of Andes system installation
// Loaded from Registry entry
// 
extern CString g_strAndesDir;
// Standard Andes Subdirectory names
extern const char* g_szLessonDir;
extern const char* g_szProblemDir;
extern const char* g_szExampleDir;
extern const char* g_szLogDir;
extern const char* g_szSolutionDir;
extern const char* g_szDemoDir;

#define ARRAY_SIZE(array_name) (sizeof(array_name)/sizeof(array_name[0]))

// helpers for version number comparisons:
extern int  MakeVersionInt(UINT n1, UINT n2, UINT n3, UINT n4=0);
extern BOOL VersionStrToInt(LPCTSTR pszVersionStr, int& nVersion);


#ifdef _DEBUG
// For use in debugger: global routine to dump current document:
extern void DumpDoc();
#endif _DEBUG


/////////////////////////////////////////////////////////////////////////////
