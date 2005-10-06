// FBD.cpp : Defines the class behaviors for the application.
//
// Main application for the ANDES Workbench.
//  
#include "stdafx.h"
#include <winver.h>
#include "FBD.h"
#include "history.h"
    
#include "MainFrm.h"
#include "ChildFrm.h"
#include "FBDDoc.h"
#include "FBDView.h"
#include "EQView.h"
#include "HelpIfc.h"
#include "EXView.h"
#include "HlpOpDlg.h"
#include "VwOptDlg.h"
#include "LoginDlg.h"
#include "TaskDlg.h"
#include "BrowsDlg.h"
#include "RuleQDlg.h"
#include "HintDlg.h"
#include "EXPlanVw.h"
#include "VarView.h"
#include "HintView.h"
#include "ChatView.h"
#include "HiLevelVw.h"
#include "TabView.h"
#include "PrincView.h"
#include "messages.h"
#include "PicCtrl.h"
#include "Splash.h"
#include "PlayDlg.h"
#include "ProblemSet.h"
#include "ProbSetView.h"
#include "ProbSetEditView.h"
#include "MDIFixedSizeFrm.h"
#include "PsmDlg.h"
#ifdef ROVING	// roving student version only
#include "TransferDlg.h"
#endif
#include "PictCtrl.h"
#include "VideoDlg.h"
#include "OliView.h"
    
#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
    
// Global variable: root of Andes directory tree, trailing backslash included 
//
CString g_strAndesDir("");	// default to none, to search relative to EXE's dir
//
// Subdirectory names, no backslash (for use in File Dialogs).
//
const char* g_szProblemDir = "Problems";
const char* g_szExampleDir = "Examples";
const char* g_szLessonDir = "Review";
const char* g_szLogDir = "Log";
const char* g_szSolutionDir = "Solutions";
const char* g_szDemoDir = "Demos";

// String constants for our registry entries
static const char* szSettingsSection = "Settings";
static const char* szInstallDir = "Install Directory";
static const char* szAuthor = "Author";
static const char* szUser = "User";
static const char* szHelpSection = "Help";
static const char* szProcHelp = "Procedural";
static const char* szConceptHelp = "Conceptual";
static const char* szExampleHelp = "Example";
static const char* szSounds = "Sounds";
static const char* szPrincipleWnd = "Principle Window";
    
/////////////////////////////////////////////////////////////////////////////
// CFBDApp
    
BEGIN_MESSAGE_MAP(CFBDApp, CWinApp)
   	//{{AFX_MSG_MAP(CFBDApp)
   	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
   	ON_COMMAND(ID_AUTHORMODE, OnAuthormode)
   	ON_UPDATE_COMMAND_UI(ID_AUTHORMODE, OnUpdateAuthormode)
   	ON_COMMAND(ID_HELP_OPTIONS, OnHelpOptions)
   	ON_UPDATE_COMMAND_UI(ID_FILE_NEW, OnUpdateFileNew)
   	ON_UPDATE_COMMAND_UI(ID_FILE_OPEN, OnUpdateFileOpen)
   	ON_COMMAND(ID_VIEW_OPTIONS, OnViewOptions)
   	ON_COMMAND(ID_FILE_EDITEXAMPLE, OnFileEditexample)
   	ON_UPDATE_COMMAND_UI(ID_FILE_EDITEXAMPLE, OnUpdateFileEditexample)
   	ON_COMMAND(ID_SHOWLESSON, OnShowlesson)
   	ON_COMMAND(ID_FILE_OPENPROBLEM, OnFileOpenproblem)
   	ON_COMMAND(ID_FILE_VIEWEXAMPLE, OnFileViewexample)
   	ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
   	ON_COMMAND(ID_SHOWRULEDLG, OnShowruledlg)
	ON_COMMAND(ID_FILE_OPENSOLUTION, OnFileOpensolution)
	ON_COMMAND(ID_FILE_OPENPROBLEMSET, OnFileOpenproblemset)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPENPROBLEMSET, OnUpdateFileOpenproblemset)
	ON_COMMAND(ID_FILE_EDIT_PROBSET, OnFileEditProbset)
	ON_UPDATE_COMMAND_UI(ID_FILE_EDIT_PROBSET, OnUpdateFileEditProbset)
   	ON_UPDATE_COMMAND_UI(ID_FILE_OPENPROBLEM, OnUpdateFileOpen)
   	ON_UPDATE_COMMAND_UI(ID_FILE_VIEWEXAMPLE, OnUpdateFileOpen)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPENSOLUTION, OnUpdateFileOpen)
	ON_COMMAND(ID_HELP_VIDEOS, OnHelpVideos)
	ON_COMMAND(ID_HELP_UNITS, OnHelpUnits)
	ON_COMMAND(ID_HELP_CONSTANTS, OnHelpConstants)
	//}}AFX_MSG_MAP
   	// Standard file based document commands
   	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
   	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
   	// Standard print setup command
   	ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
	// Custom message posted from remote event input socket
	ON_THREAD_MESSAGE(WM_EVENT_MSG, OnEventMsg)
END_MESSAGE_MAP()
    
/////////////////////////////////////////////////////////////////////////////
// CFBDApp construction
    
CFBDApp::CFBDApp()
{
   	// TODO: add construction code here,
   	// Place all significant initialization in InitInstance
   	m_bAuthorMode = FALSE;
	m_bTutorMode = FALSE;

    m_bNoHelp = FALSE;			// cmd line flag suppresses starting helpsys

   	// Following compiled in defaults chosen so can run student even if no
   	// registry entries. (Currently happens on multi-user lab machines where the
   	// registry entries are entered under the user who did the installation.)
   
   	// default to student, require reg entry to override
   	m_bAuthor = FALSE;
   	// default to full help, require reg entry to override
	m_wDefaultHelpFlags = wAllHelp;
   	m_wHelpFlags = wAllHelp; 

	m_bNoHints = FALSE;
   	m_bFeedback = TRUE;
   	m_bCheckEquations = TRUE;
   	m_bCheckDiagram = TRUE;
#ifdef HELPIFC_TCP // use sync checking when using remote tutor (for experimenting)
	m_nFeedback = FEEDBACK_WAIT;
#else
	m_nFeedback = FEEDBACK_ASYNC;	// default for EQ checking is ASYNC
#endif
   	m_nMaskMode = MM_MASK;
	m_nGreyLevel = EXViewGrey;
   	m_bUserInit = FALSE;
   	m_bTrainMode = FALSE;
	m_bDemoMode = FALSE;
	m_bPlaySounds = FALSE;
	m_bShellPrintOnly = FALSE;
	m_bRegisterOnly = FALSE;
	m_bTerminated = FALSE;
}	
    
/////////////////////////////////////////////////////////////////////////////
// The one and only CFBDApp object
    
CFBDApp theApp;

// CommandLineInfo derivative for custom command line args
class CFBDCmdLineInfo : public CCommandLineInfo
{
public:	
	// Change MFC's default of FileNew to FileNothing to suppress opening
   	// a new empty document in ProcessShellCommand below.
	CFBDCmdLineInfo()   {m_nShellCommand = FileNothing; };
protected:
	virtual void ParseParam(LPCTSTR pszParam, BOOL bFlag, BOOL bLast);
};

void CFBDCmdLineInfo::ParseParam(LPCTSTR pszParam, BOOL bFlag, BOOL bLast)
{
	// check for custom flag args first
	if (bFlag && (strcmpi(pszParam, "NoHelp") == 0)) 
	{
		theApp.m_bNoHelp = TRUE;
	} 
	else if (bFlag && (strcmpi(pszParam, "FlagMode") == 0))
	{
		// flag mode gives feedback but no help commands
		// command line overrides flags configured in registry
		theApp.m_wHelpFlags = wNoHelp; // doesn't apply to feedback
		theApp.m_wDefaultHelpFlags = wNoHelp;
	}
	else if (bFlag && (strcmpi(pszParam, "Author") == 0)
			// ! Requires author privileges in registry profile
			&& GetProfileInt(szSettingsSection, szAuthor, 0)) 
	{	
		theApp.m_bAuthorMode = TRUE; 
	}
	else if (bFlag && (strnicmp(pszParam, "Remote=", 7) == 0)) 
	{
		// invoked to function as remote ANDES viewer. Source hostname after "="
		theApp.m_strRemoteHost = strchr(pszParam, '=') + 1;

		// entails no help system on this side
		theApp.m_bNoHelp = TRUE;

		// nicety: don't show splash screen (this is a kind of server mode).
		m_bShowSplash = FALSE;
	}
	else if (bFlag && (strcmpi(pszParam, "Register") == 0))
	{
		theApp.m_bRegisterOnly = TRUE;
		m_bShowSplash = FALSE;
	}
	else // let base class parse standard args
		CCommandLineInfo::ParseParam(pszParam, bFlag, bLast);
}

/////////////////////////////////////////////////////////////////////////////
// CFBDApp initialization
    
BOOL CFBDApp::InitInstance()
{
	// Before anything else, see if there's another instance running. If so, 
	// just activate it and quit. This relies on custom WNDCLASS registration
	// implemented in CMainFrame class.
	CWnd* pWndPrev = CMainFrame::FindFBDWindow();
	if (pWndPrev) 
	{
	   CWnd* pWndPrevActive = pWndPrev->GetLastActivePopup();
	   if (pWndPrev->IsIconic())
			pWndPrev->ShowWindow(SW_RESTORE);
	   pWndPrevActive->SetForegroundWindow();

	   return FALSE;
	}

	// Parse command line, it affects how to start up:
	// CWinApp parsing recognizes the following standard command lines:
   	//		app				= FileNew	(default if no arguments)
   	//		app filename	= FileOpen
   	//      app /p filename	= FilePrint
   	//		app /pt filename ... = FilePrintTo
   	//		app /dde		= FileDDE: started to await DDE command from shell
   	//      app /automation = started as OLE automation server
   	//      app /embedded   = started as OLE embedded object server
	// We parse our own custom options via custom CmdLineInfo class above.
	CFBDCmdLineInfo cmdInfo;		// note: defaults to FileNothing.
   	ParseCommandLine(cmdInfo);
	//
	// In effect, the app can start up in one of the following modes: "interactive", 
	// for user to open and work a problem; "server" to run as an OLE server (not currently used)
	// and "batch command", to process a single command line command without further user input.
	// Currently the only batch command is to print a file from the shell, but we might add others later.
	// In non-interactive modes we start up invisibly, ie. without showing the main window. 
	// More importantly, without necessarily logging in the user and starting up and
	// connecting to the help system. However, if we were started by shell to await a 
	// DDE command, we don't know whether it's for "batch" printing or "interactive"
	// opening a file for working until the command is received in OnDDECommand below. 
	// In this case we defer user & helpsys initialization until the open command is 
	// received by OnDdeCommand.
	//
	// Following enables splash screen unless /Automation or /Embedded -- AW
	// For batch commands, we do still show the splash screen to give startup feedback.
   	CSplashWnd::EnableSplashScreen(cmdInfo.m_bShowSplash);
   
	// Could see if need help system and start it here, if nec.

#ifdef HELPIFC_TCP
	// Initialize winsock library
	if (!AfxSocketInit()) {
		AfxMessageBox(CG_IDS_SOCKETS_INIT_FAILED);
		return FALSE;
	}
#endif // HELPIFC_TCP

   	// Initialize OLE libraries
   	if (!AfxOleInit()){
   		AfxMessageBox(IDP_OLE_INIT_FAILED);
   		return FALSE;
   	}
	// Set up for OLE control containment 
   	AfxEnableControlContainer();// we contain OLE controls (MS WebBrowser)

	// Init for Richedit control use in dialog boxes
	AfxInitRichEdit();
    
   	// Standard initialization
#ifdef _AFXDLL
   	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
   	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif
   	SetRegistryKey("Andes Group"); // 
   	LoadStdProfileSettings();   // Load standard INI file options (including MRU)
   	LoadAndesSettings();		// Load our app-specific settings.
	InitVersion();				// get the fixed product version info
    	
   	// Register the application's document templates.  
	// NOTE: document type icons are registered with the shell as indices into the exe's resources.
	// RegisterShellFileTypes walks doctemplate list, assuming doctemplate ordinal parallels 
	// icon index in exe. I.e. after icon 0 for main app icon, icon 1 is registered for 
	// shell doctype type of first doctemplate, icon 2 for doctemplate 2, etc. (Parallel must
	// be maintained even if a doc template suppresses shell registration via its DocString.)
    // So must make sure icon resources are arranged so this is true, presumably by ensuring
	// first three doctype resource ID numbers parallel first three members of this list.
	CFBDDocTemplate* pDocTemplate;			// our customized tmpl preloads doc before creating frame
    
   	// normal problem type: .FBD files in an FBDView. 
   	pDocTemplate = new CFBDDocTemplate(
   		IDR_FBDTYPE,
   		RUNTIME_CLASS(CFBDDoc),
   		RUNTIME_CLASS(CChildFrame), // custom MDI child frame
   		RUNTIME_CLASS(CFBDView));
   	AddDocTemplate(pDocTemplate);
   	// Can contain embedded OLE objects. Set resources for when
   	// they are activated in-place (currently only when editing).
   	pDocTemplate->SetContainerInfo(IDR_FBDTYPE_CNTR_IP);
   	m_ptmplProblem = pDocTemplate;	// save for easy access by OpenProblem cmd
    
   	// type for example study mode -- .APX file in Example view
   	pDocTemplate = new CFBDDocTemplate(
   		IDR_EXAMPLETYPE,
   		RUNTIME_CLASS(CFBDDoc),
   		RUNTIME_CLASS(CChildFrame), 
   		RUNTIME_CLASS(CEXView));
   	AddDocTemplate(pDocTemplate);
   	m_ptmplExample = pDocTemplate;	// save for easy access by Open Example cmd

	// type for opening problem sets for working. Want this registered w/shell
	// for opening by double-clicking, but not on FileNew list
	CMultiDocTemplate* pTmpl;		// temp ptr for standard doc template
	pTmpl = new CMultiDocTemplate(
		IDR_PROBSET_TYPE, 
		RUNTIME_CLASS(CProblemSet),
		RUNTIME_CLASS(CMDIFixedSizeFrame),
		RUNTIME_CLASS(CProbSetView));
	AddDocTemplate(pTmpl);
	m_ptmplProbSet = pTmpl;		

#ifdef OLI
	// pseudo-type for task descriptor documents sent from OLI version.
	// These are a variant of problem set docs.
	// We won't really be using Doc/View on these, they're only included here 
	// so we can take advantage of MFC code for registering the filetypes.
	pTmpl = new CMultiDocTemplate(
		IDR_TASK_TYPE, 
		RUNTIME_CLASS(CProblemSet),
		RUNTIME_CLASS(CMDIFixedSizeFrame),
		RUNTIME_CLASS(COliView));
	AddDocTemplate(pTmpl);
	m_ptmplProbSet = pTmpl;		
#endif OLI

	// type for editing problem sets by instructors.
	// want this on FileNew list, since authors will create w/this template.
	// Should keep at end because shell registration is suppressed by omitting RegFileType field 
	// in document string resource, and no special icon for it in our resources. 
	pTmpl = new CMultiDocTemplate(
		IDR_PROBSETEDIT_TYPE, 
		RUNTIME_CLASS(CProblemSet),
		RUNTIME_CLASS(CMDIFixedSizeFrame),
		RUNTIME_CLASS(CProbSetEditView));
	AddDocTemplate(pTmpl);			// save for easy access by Open Example cmd
	m_ptmplProbSetEdit = pTmpl;

   	// type for editing examples -- .APX file in a normal FBD view
	// Keep at end because shell registration is suppressed by omitting RegFileType field 
	// in document string resource, and no special icon for it in our resources. 
   	pDocTemplate = new CFBDDocTemplate(
   		IDR_EXEDITTYPE,
   		RUNTIME_CLASS(CFBDDoc),
   		RUNTIME_CLASS(CChildFrame), 
   		RUNTIME_CLASS(CFBDView));
   	AddDocTemplate(pDocTemplate);
   	m_ptmplExEdit = pDocTemplate;	// save for easy access by Edit Example cmd

	// other intialization -- load kb info from external files
	CVarView::InitQuantTable();
	CPsmDlg::InitPsmInfo();
	
   	// Create main MDI Frame window but do not show it yet.
   	// On creation, Mainframe shows the splash screen if it has been enabled.
   	CMainFrame* pMainFrame = new CMainFrame;
   	if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
   		return FALSE;
   	m_pMainWnd = pMainFrame;
    
   	// Set default show cmd member to come up maximized. May be overridden
   	// from persistent state before actually showing by RestoreWindowState 
   	m_nCmdShow = SW_SHOWMAXIMIZED;
    
   	// Enable drag/drop open on the main window
   	m_pMainWnd->DragAcceptFiles();
    
   	// Enable DDE Execute open of our file types
   	EnableShellOpen();
   	RegisterShellFileTypes(TRUE);

	// if we were just running to register, we are done
	if (m_bRegisterOnly)
		return FALSE;
 	
   	// MFC expects us to call ProcessShellCommand before we show our main window.
   	// If /DDE is set, MFC's ProcessShellCommand changes the show state to SW_HIDE,
   	// saving the old one (with an ugly cast) into the m_pCmdInfo member, restoring
   	// it when the command is received in OnDDECommand. 
	// For following hidden modes, we skip ShowWindow and just process command.
	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FileDDE)
		return ProcessShellCommand(cmdInfo);

   	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FilePrint
		|| cmdInfo.m_nShellCommand == CCommandLineInfo::FilePrintTo) {
			// startup with print command: set flag to OnOpenDocument (which resets it)
			m_bShellPrintOnly = TRUE;	
   			return ProcessShellCommand(cmdInfo);
	}
   
	// special case: if running as remote viewer, start up appropriately
	if (IsRemoteViewer()) {
		// attempt to attach to the remote event server or die
		if (! LogPlayerConnectRemote(m_strRemoteHost)) {
			CString strMsg;
			strMsg.Format("Couldn't connect to %s", m_strRemoteHost);
			AfxMessageBox(strMsg);
			return FALSE;
		}
// TEMP for debugging: keep log so we can see exactly what events we received, even in Release
		HistoryFileBegin();
// END TEMP
		// always show window maximized for max display info (user can't adjust).
		pMainFrame->ShowWindow(SW_MAXIMIZE);
		return TRUE;
	}

   	// If reach here, command line had file to open or nothing. 
   	
   	// Complete user initialization before file open or prompt
   	if (! EnsureUserInit())
		return FALSE;	// back out if they cancelled login
    	
   	// Now we are ready to dispatch a file open command
   	BOOL bSucceeded = TRUE;
   	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FileOpen)
   		bSucceeded = ProcessShellCommand(cmdInfo);
    
   	// Show our main window, using persistent state if available
   	if (! pMainFrame->RestoreWindowState ())
           pMainFrame->ShowWindow (m_nCmdShow);
   	pMainFrame->UpdateWindow(); 
    
   	// If no commands were specified on command line, we open the Task dialog
   	// to prompt the user to select something to work on. 
   	if (cmdInfo.m_nShellCommand == CCommandLineInfo::FileNothing) {
   		DoInitialTaskSelect();
   	}
    	
   	return bSucceeded;
}

// This does the initial task selection if there is no task specified.
// In the past, we deferred initializing the help system until a problem was
// selected. However, now we must do that first so we can query the history
// file to see if this is a new user.
void CFBDApp::DoInitialTaskSelect()
{
	// Need helpsys to query history file
	if (!EnsureHelpSysInit()) {
		DoTaskSelect();
		return;
	}
	// else have helpsys.

#if 0 // to include video prompt in or out

	// query helpsys to see if this is a new user
	LPCSTR pszResult = HelpSystemExecf("(history-get Video)");
	if (pszResult && (strcmp(pszResult, "1") != 0))
	{
		// hasn't seen the video. Run the video prompt dialog.
		CVideoDlg dlg;
		// choices are: 0 View now; 1: Later; 2: Been there, done that.
		dlg.m_nChoice = 0;     
		int nChoice;
tryagain:
		nChoice = (dlg.DoModal() == IDOK) ? dlg.m_nChoice : 1; // Cancel => later
		if (nChoice == 0) {
			ShowVideoPage("Intro_to_Andes.html");
			goto tryagain;
		}
		else if (nChoice == 1) {
			// close the application. Should give a warning message.
			// if (AfxMessageBox("Exit Andes Now?", MB_YESNO) == IDYES) {
				::PostMessage(theApp.GetMainFrame()->m_hWnd, WM_CLOSE, 0, 0);
				return;
			//} 
			// else goto tryagain;
		}
		else if (nChoice == 2) {
			// record that they've seen it
			HelpSystemExecf("(history-set Video 1)");
			// fall through to normal task select
		}
	}
#endif

	// else failed to get info, do normal task select.
	DoTaskSelect();
}
       
// 
// LoadAndesSettings: set Andes options from values in the system registry.
//
// !!! Note: CWinApp provides several easy-to-use functions for getting/setting
// values out of the application's "profile". These originally used an application-specific 
// .ini file, but if SetRegistryKey is called (as above), the values are stored in the 
// system registry, in accordance with Windows 95 guidelines. However, in the latter case 
// MFC stores this virtual profile under HKEY_CURRENT_USER. The upshot is that the profile 
// info is now specific to the logged-in user, and no longer application-wide. Also, our 
// installer will only initialize the profile for the machine-user that did the installation. 
//
// Thus the CWinApp profile functions are suitable for user-specific preferences, but
// a different mechanism should be used for any installation-wide data such as the install
// directory. (not yet implemented. Easiest would be to use installation-wide .ini file.)
//
// Another potential issue is that multiple Andes students (as identified by the Andes login) 
// might access a single installation via the same machine account, as in the common LabUser 
// account used in one of the Pitt labs. To handle this possibility we might do well to 
// by-pass the system registry entirely and store per-student preferences with other 
// per-student info in our installation tree (perhaps in student-specific .ini files).
//
void CFBDApp::LoadAndesSettings()
{
   	// Load Pathname of Andes Installation from Registry.
   	g_strAndesDir = GetProfileString(szSettingsSection, szInstallDir, "");
	// If unset, use EXE's directory as default. (Don't just leave empty so we
	// can get full path to Andes files when needed, as by WebBrowser control.)
	if (g_strAndesDir.IsEmpty()) 
	{
		TCHAR szPathBuf[_MAX_PATH];
		VERIFY(::GetModuleFileName(m_hInstance, szPathBuf, _MAX_PATH));
		char * pLastBackslash = strrchr(szPathBuf, '\\'); // !!assumes never "C:\fbd.exe"
		if (pLastBackslash != NULL) {
			pLastBackslash[1]  = '\0';	// clobbers ch in buf after dir end
			g_strAndesDir = szPathBuf;
		}
	}
   	// Ensure it ends in backslash so can just prefix it to filenames
   	if (!g_strAndesDir.IsEmpty() && g_strAndesDir.Right(1) != "\\" )
   		g_strAndesDir += "\\";
    
   	// Load Help Type configuration flags.
   	//
   	// For booleans, we could just test for presence of key, but the MFC profile funcs
   	// don't enable testing for presence alone (were designed for INI files).
   	// So use non-zero integer value (DWORD entry) for TRUE. 
    	
   	// Following logic assumes HelpFlags initialized to all bits on (above), 
   	// using a 0 in reg to turn them *off*
   	if (! GetProfileInt(szHelpSection, szProcHelp, 1))
   		m_wHelpFlags &= ~fProcedural;
   	if (! GetProfileInt(szHelpSection, szConceptHelp, 1))
   		m_wHelpFlags &= ~fConceptual;
   	if (! GetProfileInt(szHelpSection, szExampleHelp, 1))
   		m_wHelpFlags &= ~fExample;

	// remember startup defaults in case temporarily changed by problem set
	m_wDefaultHelpFlags = m_wHelpFlags;
   	
   	// Load the author privileges flag.
  	m_bAuthor = GetProfileInt(szSettingsSection, szAuthor, 0);
   	
	// Load sound option flag
	m_bPlaySounds = GetProfileInt(szSettingsSection, szSounds, 0);
	
	// For now, principle window is experimental, configured via registry
	m_bUsePrincipleWnd = GetProfileInt(szSettingsSection, szPrincipleWnd, 0);
}

// Fetch the version number out of our .exe file, formatting into
// m_strAndesVersion string
void CFBDApp::InitVersion()
{
	DWORD dwVerInfoSize;
	DWORD dwHwnd;
	void* pBuffer;
	TCHAR szExeName[_MAX_PATH];
	UINT  uVersionLen = 0; 
	LPVOID  lpVersion = NULL; 
	VS_FIXEDFILEINFO *pFixedInfo; // pointer to fixed file info structure

	VERIFY(::GetModuleFileName(AfxGetInstanceHandle(), szExeName, sizeof(szExeName)));
	dwVerInfoSize = GetFileVersionInfoSize(szExeName, &dwHwnd);
	if (dwVerInfoSize) { 
		pBuffer = malloc(dwVerInfoSize); 
		if (pBuffer == NULL)
			return;
		GetFileVersionInfo(szExeName, dwHwnd, dwVerInfoSize, pBuffer);
		VerQueryValue(pBuffer,_T("\\"),(void**)&pFixedInfo,(UINT *)&uVersionLen); 
		CString strFixedProductVersion;
		CString strFixedFileVersion;
		m_strAndesVersion.Format ("%u.%u.%u", HIWORD (pFixedInfo->dwProductVersionMS),
			              LOWORD (pFixedInfo->dwProductVersionMS),
			              HIWORD (pFixedInfo->dwProductVersionLS));
		// display last number only if non-zero. Set it for pre-release updates, tests.
		if (LOWORD (pFixedInfo->dwProductVersionLS)) {
			m_strAndesVersion.Format("%s.%u", m_strAndesVersion,
											LOWORD (pFixedInfo->dwProductVersionLS));
		}
	//	strFixedFileVersion.Format ("%u,%u,%u,%u",HIWORD (pFixedInfo->dwFileVersionMS),
	//		             LOWORD (pFixedInfo->dwFileVersionMS),
	//		             HIWORD (pFixedInfo->dwFileVersionLS),
	//		             LOWORD (pFixedInfo->dwFileVersionLS)); 
 
		if (pBuffer != NULL)
			free(pBuffer);

	}
/*
	// Also set the OS version flag here
	OSVERSIONINFO osvi;
	osvi.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	if (::GetVersionEx ( &osvi) ) {	
		// dwPlatformId == VER_PLATFORM_WIN32_WINDOWS:
		//     [dwMajorVersion == 4]
		//     dwMinorVersion = 0 => Win95
		//     dwMinorVersion > 0 => Win98 or Me
		//           98: minor==10; 
		//           Me: minor==90  
		// dwPlatformId == VER_PLATFORM_WIN32_NT
		//      dwMajorVersion, dwMinorVersion:
		//          3.51 => NT 3.51
		//          4.0  => NT 4.0
		//          5.0  => Windows 2000
		//          5.1  => Windows XP	
	}
*/
}

// Although stored version resources have 4 16 bit slots allowing 64 bits total,
// on assumption no slot is >255, we can convert 4 numbers to 32-bit int for easy comparison.
// Use to make a specific version number for comparison, e.g: MakeVersionInt(7,0,4);
int MakeVersionInt(UINT n1, UINT n2, UINT n3, UINT n4/*= 0*/)
{
	ASSERT(n1 <= 255);
	ASSERT(n2 <= 255);
	ASSERT(n3 <= 255);
	ASSERT(n4 <= 255);
	return MAKELONG(/*low*/  MAKEWORD(/*low*/n4, /*high*/n3),
	                /*high*/ MAKEWORD(/*low*/n2, /*high*/n1));
}

// Helper for converting version number strings of form N1.N2.N3[.N4]
// to integers for easy comparison of version numbers
// RETURNS: TRUE for success, filling in nVersion; FALSE on error
extern BOOL VersionStrToInt(LPCTSTR pszVersionStr, int& nVersion)
{
	int n1=0, n2=0, n3=0, n4=0;
	if (sscanf(pszVersionStr, "%d.%d.%d.%d", &n1, &n2, &n3, &n4) < 3) {
		return FALSE;
	}	
	nVersion = MakeVersionInt(n1, n2, n3, n4);
	return TRUE;
}



// For managing global glossary of physics defs for popup definitions in hints.
BOOL CFBDApp::LookupDef(LPCTSTR pszKey, CString& strDef)
{
	// Load glossary as needed
	if (m_mapPhysicsDefs.IsEmpty())
		LoadGlossary();

	return m_mapPhysicsDefs.Lookup(pszKey, strDef);
}

void CFBDApp::LoadGlossary()		// read in glossary file format		
{
	//Glossary.txt file located in Install Directory\pData
	CString strPdataDir = g_strAndesDir + "pdata\\";
	CString strPathName = strPdataDir + "Glossary.txt";
	TRACE("Loading glossary file %s\n",strPathName);
	char pBuffer[10000];
	try {
		CFile file(strPathName, CFile::modeRead);
		UINT nBytesRead = file.Read(pBuffer, 10000);
	
		char * token = strtok(pBuffer,"\"");
		while (token != NULL){
			CString term = token;
			term.TrimRight();
			term.TrimLeft();
		    token = strtok(NULL,"\"");
			CString def = token;
			token = strtok(NULL,"\"");
	
			//one-to-one relationship,  word-def	
			m_mapPhysicsDefs.SetAt(term, def);
			TRACE("Map %s to %s\n", term, def);
		}
	}
	catch (CFileException* e) {
		if (e->m_cause == CFileException::fileNotFound)
			theApp.DoWarningMessage("Glossary file not found: Glossary.txt");
		else
			theApp.DoWarningMessage("Error opening Glossary file: Glossary.txt");
		e->Delete();
	}
}

//
// EnsureUserInit -- ensure have completed initialization needed for 
//           interactive use 
//
// Starts history log, starts help system if needed and connects, collects
// user login and submits to help system. Currently Help System initialization
// must be done before any opening of a document, so OnOpenDocument can use
// it to send problem name. 
//
// This initialization not needed if running "non-interactively" to process a 
// print command. 
// 
// As name implies, this routine may safely be called repeatedly.
// 
BOOL CFBDApp::EnsureUserInit()
{
	// no-op if already done
   	if (m_bUserInit)	
   		return TRUE;

   	// Begin History log. Want it started before initial help system 
   	// operations so they can be traced in the log, if desired
   	HistoryFileBegin();

	// Make sure help system is running, if required
	// We can ensure connected and initialized later.
	if (! m_bNoHelp && ! HelpSystemEnsureRunning()) 
		// Session aborts, should notify user here!
		return FALSE;
    
	// Unless we've already set student name from elsewhere 
	// (mainly OLI-sent task document, maybe command line)
   	if (m_strUserName.IsEmpty()) 
	{
   		// Collect student's login name to send to help system
		CLoginDlg dlg(NULL, m_pMainWnd);
   		// initial default is last login name saved in registry
   		dlg.m_strName = GetProfileString(szSettingsSection, szUser, "");
   		if (dlg.DoModal() != IDOK)	// cancelled dialog
			return FALSE;

		m_strUserName = dlg.m_strName;
	
   	// save in registry for use next time 
   	WriteProfileString(szSettingsSection, szUser, m_strUserName);
    }

   	// Rename history file w/student name. Remember name for use
	// as unique Andes session id, for easy correlation with logs
   	m_strSessionId = HistoryFileSetStudent(m_strUserName);

	// If this is roving student installation, download student files now
	if (IsRovingStudent())
		TransferStudentFiles(/*bUpload = */ FALSE);
   	
	// update flag and return
   	return m_bUserInit = TRUE;
}
    
//
// DDE execute command handler for the application.
//
// MFC apps accept execute strings for application on application exe-name and
// topic "system". Base class handles standard commands from shell to print,
// open, or printto a printer. We handle further commands in our logging
// notation by passing to the log message dispatcher.
//
// If /dde set on command line, then we were started up invisibly to await 
// receipt of startup command from shell via DDE. But can receive further
// (i.e. non-startup) DDE commands from shell or helpsys at any time.
//
BOOL CFBDApp::OnDDECommand(LPTSTR lpszCommand) 
{
	// See if this is a standard print command
	// if so, don't need login or help system. but EnsureHelpSysInit hook is
	// called on opening of document. So we set flag to communicate open purpose 
	// to only ensure help init if open for working. Flag is cleared when
	// consumed by OnOpenDocument
	CString strCommand = lpszCommand;
   	m_bShellPrintOnly = ( strCommand.Left(8) == "[print(\"" 
   	                   || strCommand.Left(10) == "[printto(\"" );
	// see if it's an open command on a task document. if so, may not need login.

	BOOL bTaskOpen = strCommand.Left(7) == "[open(\"" 
					 && (strCommand.Find(".atd") != -1 
					     || strCommand.Find(".ATD") != -1); 

	// if we deferred normal startup user login pending DDE command
	if (! m_bUserInit)	
   	{
   		// complete user initialization (login) unless it's just a print command
		// or a task open, which may contain user name
		if (! (m_bShellPrintOnly || bTaskOpen)) {
			// shell open command: make sure student is logged in. 
			// What to do if they cancel? If we don't handle the command,
			// we are orphaned in a bad state, running invisibly (w/help system running
			// and unconnected to boot!). Need to exit or show; for now, quit.
			if (! EnsureUserInit()) {
				::PostQuitMessage(0);
				return FALSE;
			}
		}
	}
    
   	// Allow base class implementation to consume standard DDE shell commands
   	if (CWinApp::OnDDECommand(lpszCommand) ) 
   		return TRUE;
   
	// Else handle command, assumed from the local help system 
	return OnHelpSysCommand(lpszCommand);
 }

BOOL CFBDApp::OnHelpSysCommand(LPCSTR pszCmd)
{
#ifdef HELPIFC_TCP
	// Hack for Wizard of OZ exps: make sure any partially typed contents have been 
	// logged before processing (and logging) command to add tutor text
	static const char szShowHintCmd[] = "show-hint";
	CChatView* pChatView = GetChatView();
	if (pChatView && _strnicmp(pszCmd, szShowHintCmd, sizeof(szShowHintCmd) - 1) == 0)
		pChatView->LogUnsentText();
#endif 

	// log all commands from help system here (ExecuteCmd is worker so doesn't log).
	LogEventf(EV_DDE_CMD, "%s", pszCmd);

	// and delegate to the cmd msg dispatcher for log playback (in history.cpp).
   	return ExecuteCmd(pszCmd);
}

// In case we also accept commands over TCP/IP port.
// Custom msg posted from the Remote Viewer input socket.
// Signals that an input cmd is available in the "input queue" for processing
LONG CFBDApp::OnEventMsg(UINT wParam, LONG lParam)
{
	LogPlayerProcessInput();
	return 0L;
}

// Ensure help system is connected and initialized for student session.
// Now separate from user login so can defer wait for connection until help 
// system is actually needed for a problem. 
// Should call this before opening a problem for working.
BOOL CFBDApp::EnsureHelpSysInit()
{
	// no-op if already initialized 
	if (m_bHelpSysInit) return TRUE;

	// no initialization to do if running without help system
	if (m_bNoHelp) return m_bHelpSysInit = TRUE;

	// only call this after student  is logged in
	if (! EnsureUserInit())
		return FALSE;
	
	// Set up IPC connection to help system	
   	if (!HelpSystemConnect()) 
	{
   		DoWarningMessage("Couldn't connect to Help system.\r\nAlthough you may still work, feedback, hints and solver will not be available");
   	} 
	else // try initial call to help system
	{ 
	    // New: Andes 8.0.0.1: inform help system of the logfile name, for use as session id.
	   HelpSystemSendf( "(set-session-id \"%s\")", m_strSessionId);
		// (read-student-info "name" Conc-help [1 or 0 as Conceptual help on or off]
	   if ( ! HelpSystemExecf("(read-student-info \"%s\" %d)", 
		                            LISPSTR(m_strUserName), 
   				                    (m_wHelpFlags & fConceptual) ? 1 : 0) ){
		// are getting unexplained failures here on Win2000 systems, even though appear to be connected
		// is some other process using the TCP/IP port? Is an orphaned help system not processing events?
		DoWarningMessage("Failed to initialize Help system!\r\nYou may work, but feedback and hints will not be available.  \
If this happens repeatedly, restarting your computer may remove the problem.");
		// shutdown connection so we don't try again on problem open.
		HelpSystemDisconnect();
	   }
	} 

   	// Note helpsys initialization marked complete even if failed,
   	// Will just run w/o feedback.
	return m_bHelpSysInit = TRUE;
} 

// Shutdown processing:
    
// Hook into framework's "SaveAllModified" which is invoked on app shutdown.
// (although it can be invoked anywhere).
// If frame's m_bClosing flag is set to indicate frame is in process of closing
// (app is shutting down), we must clear it if SaveAllModified fails or is cancelled.
// Else this marks point where we know app is shutting down for sure, and we take
// some cleanup actions here, rather than in the mainframe OnClose or in ExitInstance,
// which is too late to put up a dialog.
BOOL CFBDApp::SaveAllModified() 
{
   	// all the prompt/save work happens in the base class
   	BOOL bAllSavedOK = CWinApp::SaveAllModified();
    	
   	// take special action if this is part of MainFrame shutdown sequence
   	CMainFrame* pFrame = (CMainFrame*) m_pMainWnd;
   	if (pFrame && pFrame->m_bClosing) // called after shutdown initiated. 
   	{	
   		if (!bAllSavedOK)	// we're not going to shutdown
   		{
   			// clear App closing flag in frame.
   			pFrame->m_bClosing = FALSE;
   			LogEventf(EV_CANCEL_CLOSE, "");
   			return bAllSavedOK;		// do no more 
   		}

   		// If get here => we are really shutting down app. Default shutdown sequence after
		// call here (in CFrameWnd::OnClose) will do:
		//		CWinApp::HideApplication   -app window invisible for immed. feedback 
		//		CWinApp::CloseAllDocuments (not virtual!) - close all open docs
		// Since we notify helpsys on document close, this means possibly long waits for 
		// help system calls to complete can occur while app continues to run invisibly in
		// the shutdown process. Though generally OK (and Andes2 is much faster to shutdown
		// then Andes1 with Bayes net updating), it could be bad to continue invisibly
		// -- student could launch another session which could connect to helpsys, e.g.
		// So to make any possible shutdown wait visible, we close all documents here 
		// and do final cleanup before returning.
		// NB: This is also prerequisite for uploading student files at this point, so
		// log file is closed, and helpsys student file are written to disk for uploading,
		// (Helpsys student file is not written out until Andes session ended.)
		CloseAllDocuments(/* bEndSession = */FALSE); // flag means not ending Windows 
		DoFinalCleanup();

		// for Pitt experiment students roving over the network, run the student file dlg
		// to upload all student files to a server on session end. 
		if (IsRovingStudent()) {
			TransferStudentFiles(/*bUpload=*/TRUE);
		}

		// For demo ANDES: never upload. Put back in for USNA eval
#if 0
		// For USNA evaluation ANDES, prompt students to run the data uploader utility (default)
		// Don't nag author/developers, though, and allow override of default via reg setting 0.	
		if (! m_bAuthor && theApp.GetProfileInt(szSettingsSection, "Upload", 1))
			QueryUpload();
#endif 
	}
   	return bAllSavedOK;
}
    
// Prompt user to upload Andes experimental data
BOOL CFBDApp::QueryUpload()
{
 	int nResult = IDYES;
#if 0	// for Fall 99 Eval: don't even query, run uploader after every session

   	nResult = AfxMessageBox("Would you like to upload Andes' experimental data now?",
   							 MB_ICONQUESTION | MB_YESNO);
   	if (nResult != IDYES)
   		return FALSE;
#endif 0 
   	// else start the uploader utility. This is a separate process and may run
   	// while the problem close is waiting for the assessor to update.
   
   	// In this case we end history logging now, so that log of this session will be 
   	// available for uploading even while awaiting problem close. The log will lose 
   	// the last couple of calls to the help system, but that;s better than having
   	// an error in the upload. HistoryFileEnd is also called in InitInstance to handle
   	// case where student's decline. It is safe to call multiple times.
   	LogEventf(EV_LAUNCH_UPLOADER, "");
   	HistoryFileEnd();
    
   	// Attempt to start the uploader process.
   	CString strExePath = g_strAndesDir + "Upload.exe";
   	HINSTANCE hInst;
   	hInst = ShellExecute(NULL, "open",  strExePath, NULL, NULL,  SW_SHOWNORMAL); 
   	if ((int) hInst <= 32) {
   		TRACE("Shellexec failed, code = %d\n", (int) hInst);
   		AfxMessageBox("Couldn't execute Upload.exe", MB_ICONERROR);
   	}
   
   	return (nResult == IDYES);
}

// Following tests if current session should support "roving" student by saving
// student state files over the network around the session. This is not controlled 
// by registry entry since students on the net in the campus labs will be running 
// out of a quick and cheap install which just unzips into a temp dir but doesn't make 
// registry entries. We control by looking for a marker file which we only put in our 
// experimental version network installation.
BOOL CFBDApp::IsRovingStudent()
{
#if ROVING	// just compile it into this version
	/*CFileStatus statFile;
	return CFile::GetStatus(g_strAndesDir + "GettingStarted.doc", statFile));*/
	return TRUE;
#else
	return FALSE;
#endif

}

// Move student files to/from server machine, direction determined by flag.
// work is done in relevant dialog.
void CFBDApp::TransferStudentFiles(BOOL bUpload)
{
	// if uploading at end, can hide application now, small dialog will show instead
	if (bUpload)
		HideApplication();

#ifdef ROVING
	CTransferDlg dlg;
	dlg.m_bUpload = bUpload;
	dlg.m_strStudent = m_strUserName;
	dlg.DoModal();
#endif ROVING
}

// Perform final cleanup actions just prior to end of Andes session 
// Shuts down helpsys and closes history file. 
// Safe to call multiple times just to be sure.
void CFBDApp::DoFinalCleanup()
{
	// no-op if already done
	if (!m_bTerminated) 
	{
		CWaitCursor wait;	//  show hourglass if mainwnd still visible.

		// Shut down help system connection at exit 
   		HelpSystemDisconnect();
    
   		// Finish history log 
   		HistoryFileEnd();

		// don't do again
		m_bTerminated = TRUE;
	}
}

int CFBDApp::ExitInstance() 
{
	// Just in case, make sure we have done final cleanup
	DoFinalCleanup();   
       
   	// Following is to block an obscure bug in MFC. Base class's ExitInstance
   	// tries to delete storage pointed to by its m_pCmdInfo if it finds it non-Null. 
   	// As far as I can tell this should never happen as MFC is currently implemented,
   	// since the App never allocates this memory. However, we found it to occur if 
   	// the App was started for DDE open, in which case ProcessShellCommand cached the
  	// original show state there, but the expected DDE command never arrived, presumably
   	// because we took too long to start up and the shell timed out.
   	//
   	// This doesn't happen any more since we changed to defer initialization when 
	// started with /dde to await cmd from shell. Still can't hurt to make sure.
   	m_pCmdInfo = NULL;

	// Defense against logic bugs: before quitting, ensure we have closed all open 
	// mci devices, such as sound device opened for audio log record/playback. Appears
	// exiting with open mci devs can leave the mmsystem wedged, requiring reboot to clear.
	DoMciSendString("close all");
    
   	return CWinApp::ExitInstance();
}
    	
    
/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About
    
class CAboutDlg : public CDialog
{
public:
   	CAboutDlg();
	CPictureCtrl m_wndPict;
	CStaticLink m_lnkAndes;
	CStaticLink m_lnkEmail;
    
// Dialog Data
   	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	CString	m_strVersion;
	//}}AFX_DATA
   
   	// ClassWizard generated virtual function overrides
   	//{{AFX_VIRTUAL(CAboutDlg)
   	protected:
   	virtual void DoDataExchange(CDataExchange* pDX);// DDX/DDV support
   	//}}AFX_VIRTUAL
    
// Implementation
protected:
   	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
   	DECLARE_MESSAGE_MAP()
};
    
CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
   	//{{AFX_DATA_INIT(CAboutDlg)
	m_strVersion = _T("");
	//}}AFX_DATA_INIT
}
    
void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
   	CDialog::DoDataExchange(pDX);
   	//{{AFX_DATA_MAP(CAboutDlg)
	DDX_Text(pDX, IDC_VERSION, m_strVersion);
	//}}AFX_DATA_MAP
}
    
BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
   	//{{AFX_MSG_MAP(CAboutDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL CAboutDlg::OnInitDialog() 
{
	m_wndPict.SubclassDlgItem(IDR_ONR_LOGO, this);
	m_lnkAndes.SubclassDlgItem(IDC_ANDES_URL, this);
	m_lnkEmail.SubclassDlgItem(IDC_ANDES_EMAIL, this, "mailto:andesits@pitt.edu");

	return CDialog::OnInitDialog();
}
  
// App command to run the dialog
void CFBDApp::OnAppAbout()
{
   	CAboutDlg aboutDlg;
	aboutDlg.m_strVersion = m_strAndesVersion;
   	aboutDlg.DoModal();
}
    
/////////////////////////////////////////////////////////////////////////////
// CFBDApp commands
    
// Toggle app in and out of author mode, in which editing commands for authors
// are available.
//
// We require a nonzero registry entry under "Author" to mark our configuration as an 
// author installation. This gives user right to enter author mode.
//
// !!! This probably would be better as a per-view attribute. That is,
// authors could have a command to open an edit-mode view on a document.
// For an already-open problem that could be either by replacing views in the current frame 
// (see samples in Kruglinski or Microsoft sample) or by opening in a new frame window. 
// If we do this, it might also be useful to make edit-mode view a separate sub-class.
// Note that since we allow authors to open multiple windows at once (see below), they
// might have both an edit-mode and a student-mode view open at the same time, which could
// be more of a nuisance for us (e.g. if they add or delete elements like answer-boxes or 
// choice buttons, which require creating run-time controls -- not handled now).
//
// For now we stick to simple approach in which the whole app enters edit mode, and
// the global flag affects interpretation of certain commands.
//
void CFBDApp::OnAuthormode() 
{
    if (! m_bAuthor) {			// user doesn't have authoring privileges
    	m_bAuthorMode = FALSE;	// Shouldn't ever happen.
		return;
	}
     
    // toggle author mode
    m_bAuthorMode = ! m_bAuthorMode;
    
    // show/hide the Author palette (toolbar w/graphics drawing & other author cmds)
    ((CMainFrame*)m_pMainWnd)->ShowAuthorBar(m_bAuthorMode);
    
	// for current doc, broadcast a hint notifying all views of mode change.
	CDocument* pDoc = GetDocument();
	if (pDoc != NULL) 
		pDoc->UpdateAllViews(NULL, HINT_AUTHOR_MODE, (CObject*) m_bAuthorMode);
	// !!! author could have opened multiple docs while in author mode.
	// should ensure only one is "current document" when return to student mode

	// ? Need to suspend feedback if help system connected? Should be OK to keep.
	// Only problem is if multiple problems get opened.
}
    
void CFBDApp::OnUpdateAuthormode(CCmdUI* pCmdUI) 
{
    pCmdUI->Enable(m_bAuthor);
    pCmdUI->SetCheck(m_bAuthorMode);
}

// 
// Go in or out of Tutor Dialog mode
// 
void CFBDApp::SetTutorMode(BOOL bTutorMode)
{
	// no-op if mode not changed
	if (m_bTutorMode == bTutorMode) 
		return;
	
	// else change it and broadcast notification to all views.
	m_bTutorMode = bTutorMode;
	CDocument* pDoc = GetDocument();
	if (pDoc != NULL) 
		pDoc->UpdateAllViews(NULL, HINT_TUTOR_MODE, (CObject*) m_bTutorMode);

	// also notify current dialog, if its one of ours
	// Current log dialog should have registered itself on startup into
	// log player state, so we can find it easily from there.
	CDialog* pDlg = LogPlayerGetCurrentDlg();
	if (pDlg && pDlg->IsKindOf(RUNTIME_CLASS(CCheckedDlg)))
		((CCheckedDlg*)pDlg)->OnTutorModeChange(m_bTutorMode);
}

//////////////////////////////////////////////////////////////////////////////////////
// File menu operations:
//
// Standard file open command implementations are in CWinApp; Save/Save As/Close are 
// handled by CDocument.
//
// We must hook the framework's implementations to enforce file opening constraints:
// We only allow students to open one problem at a time (the help system 
// requires that). So we disable the command if in student mode and there's already a problem 
// open. We allow authors to edit multiple problems at once.
// Following public method implements the test:
//
BOOL CFBDApp::CanOpenProblem()	// test if OK to open a new problem file
{ 
    // Allow to students just in case no currently active problem
	// i.e. no active MDI Child ("document") window
    // Special case allows open in demo mode (must come from demo script).
	// Note caller should skip this test to open for purpose of previewing file.
	return (m_bAuthorMode || m_bDemoMode || m_pMainWnd == NULL ||
    	    GetDocument() == NULL );
}
    
void CFBDApp::OnUpdateFileOpen(CCmdUI* pCmdUI) // update file open command
{
	// This handler also used for all commands to open individual files.
	// Also don't allow students to open "loose" files if there is a
	// current problem set. Can't pack this  into CanOpenProblem since that check 
	// gets called even when opening *from* a problem set. So it is still possible
	// for them to open a loose problem w/pset open via MRU/DragDrop/ShellDDE.
	// But graying menu should discourage them.
    pCmdUI->Enable ( CanOpenProblem() && (GetProblemSet() == NULL || m_bAuthorMode) );
}

//   
// A nuisance is that MFC AppWizard-generated MDI apps enable several other standard ways
// of asking the app to open a file, which are not all routed through a common command handler: 
// 
// (1) double-clicking a file icon on the desktop for a registered file type (shell
// sends a DDE "open" command handled in CWinApp::OnDdeCommand); 
// (2) dragging and dropping a filename from the shell onto our main frame window; 
// (enabled by DragAcceptFiles; handled by CFrameWnd::OnDropFiles);
// (3) selecting the file name from the most-recently-used (MRU) list on the file
// menu (marked by ID_MRU_FILE1, which is expanded into up to 16 names in 
// CWinApp::OnUpdateRecentFileMenu; command handled in CWinApp::OnOpenRecentFile). 
//
// We would like to keep these methods available while student is *between* problems but 
// block them while students are working on a problem. Turns out all methods by which
// users request file open funnel control through the following worker routines: 
//
// CFBDApp::OpenDocumentFile 
//		if file is already open in app then activate it; 
//		else find the appropriate doc template and call
//			CDocTemplate::OpenDocumentFile. 
//           create new document, frame (creates client) and view and call
//					CFBDDocument::OnOpenDocument to stream in the document from the file
//
// So we put our test in the worker routine here. However, note that we might someday want to be
// able to open a second problem document programmatically even while in student mode, e.g. 
// to show an example solution, side by side with the student's problem, in which case that
// test would have to change -- might need flag to set purpose of document open, whether
// user opened for working or program opened for other reason. This is already done when
// opening for handling a shell print command via the m_bShellPrintOnly flag.
//
// Following does the work of opening a file for application. Default
// checks if document is already open, activating if it is; else searches to
// find appropriate document template and asks template to do it.  
CDocument* CFBDApp::OpenDocumentFile(LPCTSTR lpszFileName) 
{
	// Identify doc type by extension
	LPCTSTR pszExt = strrchr(lpszFileName, '.');
	ASSERT(pszExt != NULL);
	if (strcmpi(pszExt, ".fbd") == 0) // problem, example, or solution
	{
		// make sure problem open is permitted in this state;
		// No need to check if only  opening to handle shell print command
		if (! (m_bShellPrintOnly || CanOpenProblem()) ) 
		{
			DoWarningMessage("Only one problem may be open at a time. \n\
Close current problem before opening a new one.");
    		
			return NULL;
		}
	} 
	else if (strcmpi(pszExt, ".aps") == 0) // problem set
	{
		// enforce constraints on problem set open here as well.
		if (! CanOpenProblemSet()) {
			DoWarningMessage("You must close current problem and problem set before opening a problem set\n");
    		return NULL;
		}
	}
	else if (strcmpi(pszExt, ".atd") == 0) {
		if (! CanOpenProblemSet()) {
			DoWarningMessage("You must finish and close your current activity before you can start a new one\n");
    		return NULL;
		}
	}
	
	// let base class do the work of opening via appropriate doc template
	return CWinApp::OpenDocumentFile(lpszFileName);
}
    
//
// Only an author can use the file New command
//
void CFBDApp::OnUpdateFileNew(CCmdUI* pCmdUI) 
{
    pCmdUI->Enable(m_bAuthorMode);
}
    
//
// File command handlers :
//
    
// Generic file open command: Allows opening of any of our file types. using
// standard dialog. Available in Student mode, but translates to our Task dialog
void CFBDApp::OnFileOpen() 
{
    // In Student Mode, run the task selection dialog to choose which type
    // of file to open.
    if (! theApp.m_bAuthorMode) {
    	DoTaskSelect();
    	return;
    }
    // for authors, just delegate to the framework's implementation
    // Allows choice of registered doc types, and starts in current directory, not
    // necessarily an Andes directory.
    CWinApp::OnFileOpen();
}
    
// Special open command to open a problem file
void CFBDApp::OnFileOpenproblem() 
{
	// !! For consistency, should extract parms from template string
    CPicCtrl dlg(TRUE, "fbd","*.fbd");
	//	OFN_HIDEREADONLY | OFN_NOCHANGEDIR /*|OFN_ENABLETEMPLATE | OFN_EXPLORER*/,
    			//	"Andes Physics Problems (.fbd)|*.fbd||");
    // Start the dialog in Problems directory.
	CString strDir = g_strAndesDir + g_szProblemDir;
	dlg.m_ofn.lpstrInitialDir = strDir;
    if (dlg.DoModal() != IDOK)
    	return;
    	
    // open selected file using appropriate doc template
    CString strPathName = dlg.GetPathName();
    m_ptmplProblem->OpenDocumentFile(strPathName);
}
// Special open command to open an previously saved, partially
//solved, student solution
void CFBDApp::OnFileOpensolution() 
{
	// !! For consistency, should extract parms from template string
    CPicCtrl dlg(TRUE, "fbd","*.fbd");
	//	OFN_HIDEREADONLY | OFN_NOCHANGEDIR /*|OFN_ENABLETEMPLATE | OFN_EXPLORER*/,
    			//	"Andes Physics Problems (.fbd)|*.fbd||");
    // Start the dialog in Problems\\Solutions directory.
	CString strDir = g_strAndesDir + g_szProblemDir + "\\" + g_szSolutionDir;
	dlg.m_ofn.lpstrInitialDir = strDir;
    if (dlg.DoModal() != IDOK)
    	return;
    	
    // open selected file using appropriate doc template
    CString strPathName = dlg.GetPathName();
    m_ptmplProblem->OpenDocumentFile(strPathName);
	
}

// Special open command to open an Example file for viewing
void CFBDApp::OnFileViewexample() 
{
    // !! For consistency, should extract parms from template string
    CFileDialog dlg(TRUE, "apx", NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "Andes Physics Examples (.apx)|*.apx||");
    // Start the dialog in the Examples
    CString strDir = g_strAndesDir + g_szExampleDir;
    dlg.m_ofn.lpstrInitialDir = strDir;
    
    if (dlg.DoModal() != IDOK)
    	return;
    	
    // open selected file using appropriate doc template
    CString strPathName = dlg.GetPathName();
    m_ptmplExample->OpenDocumentFile(strPathName);
}

// Special open command for authors to open an example file in an editing view:
void CFBDApp::OnFileEditexample() 
{
    // get the example file name with file open dialog
    CFileDialog dlg(TRUE, "apx", NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "Andes Physics Examples (.apx)|*.apx||");
    CString strDir = g_strAndesDir + g_szExampleDir;
    dlg.m_ofn.lpstrInitialDir = strDir;
    if (dlg.DoModal() != IDOK) 
    	return;
    
    // open selected file using appropriate doc template
    CString strPathName = dlg.GetPathName();
    m_ptmplExEdit->OpenDocumentFile(strPathName);
}
    
void CFBDApp::OnUpdateFileEditexample(CCmdUI* pCmdUI) 
{
    pCmdUI->Enable(m_bAuthorMode);
}

void CFBDApp::OnFileOpenproblemset() 
{
	CFileDialog dlg(TRUE, "aps", NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "Andes Problem Set (.aps)|*.aps||");
    // Start the dialog in the Problem Dir
    CString strDir = g_strAndesDir + g_szProblemDir;
    dlg.m_ofn.lpstrInitialDir = strDir;
    
    if (dlg.DoModal() != IDOK)
    	return;
    	
    // open selected file using appropriate doc template
    CString strPathName = dlg.GetPathName();
    m_ptmplProbSet->OpenDocumentFile(strPathName);
}

BOOL CFBDApp::CanOpenProblemSet()
{
	// Can open new problem set if no problem set currently open
	// (and no current problem either, in case allow singleton problem open).
	return (! (GetProblemSet() || GetCurrentProblem()));
}
void CFBDApp::OnUpdateFileOpenproblemset(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(CanOpenProblemSet());
}

void CFBDApp::OnFileEditProbset() 
{
	CFileDialog dlg(TRUE, "aps", NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "Andes Problem Set (.aps)|*.aps||");
    // Start the dialog in the Problem Dir
    CString strDir = g_strAndesDir + g_szProblemDir;
    dlg.m_ofn.lpstrInitialDir = strDir;
    
    if (dlg.DoModal() != IDOK)
    	return;
    	
    // open selected file using appropriate doc template
    CString strPathName = dlg.GetPathName();
    m_ptmplProbSetEdit->OpenDocumentFile(strPathName);
}

void CFBDApp::OnUpdateFileEditProbset(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_bAuthorMode);
}

void CFBDApp::DoTaskSelect()	// select next task to work on
{
	// temporary: can disable problem set code w/reg entry
	if (theApp.GetProfileInt(szSettingsSection, "NoProblemSets", 0)) {
		CTaskDlg dlgTask;
   		(void) dlgTask.DoModal();
		return;
	}
	// else use new problem-set method

	// see if we have a problem set
	CProblemSet* pSet = GetProblemSet();
	if (pSet == NULL) {	// if no current problem set, prompt to open one
		OnFileOpenproblemset();
	} else if (pSet->m_bOli) {
		// if it's an auto-open task and we get here, just close the set and 
		// leave screen blank (could shut down Andes)
		pSet->OnCloseDocument();
	} else 	{ 
		// show the problem set view's frame to select next problem
		CProbSetView* pView = GetProbSetView();
		ASSERT(pView != NULL);
		// View will show current session state as updated on close of document,
		// even if user hasn't saved and state won't persist across sessions.
		// Could do pSet->UpdateSolutionState here to show persistent state instead.
		pView->GetParentFrame()->ShowWindow(SW_SHOWNORMAL);
		
		// Ensure we are in the foregound, since activation may have bounced 
		// on close of problem frame (happens when running Tutor in Lisp IDE).
		::SetForegroundWindow(pView->GetParentFrame()->GetSafeHwnd());
		pView->GetParentFrame()->ActivateFrame();
	}
}

/////////////////////////////////////////////////////////////////////////////////
// UI services for the tutor.
// Most are made accessible to the help system via our DDE interface
// by appropriate entries in the command dispatch table in history.cpp.
/////////////////////////////////////////////////////////////////////////////////
    
// ShowLesson: Worker routine to display an html file in our browser
// optional arg is usually lesson filename taken relative to Andes Lesson directory
// Added -- accept arbitrary Web URL as well, identified by presence of colon.
// Note: no-op if no help system message window currently exists.
    
// Filename is optional, opens contents page if none specified
static const char* szDefaultPage = "CONTENTS.HTM";
    
void CFBDApp::ShowLesson(LPCTSTR pszName /* = NULL*/)
{
	// It's possible cmd comes in response to submission of initial entries on
	// problem open (see FBDDoc::CheckInitEntries) before the problem wnd is shown.
	// We ignore it in this case by not doing anything unless a help system message 
	// window currently exists, in effect treating show-lesson like show-hint. 
	// Msg wnd doesn't exist while checking initial entries since our custom doc 
	// template opens document *before* creating frame and views, and we check on open.
	// For programmatic use, set the FORCE flag to true
	if (theApp.GetChatView() || theApp.GetHintView())
		DoShowLesson(pszName);
}

// This does the work, and is also for internal programmatic use
void CFBDApp::DoShowLesson(LPCTSTR pszName)
{
	//show it in our custom browser dialog
    // !!! Pefer browser to be modeless? If so, check if browser already up 
    CString strPage = pszName ? pszName : szDefaultPage;
    
	// If page name contains colon, assume it contains a full URL; 
	// else an ANDES-Review-relative file URL.
	if (strPage.Find(':') != - 1)
		m_dlgLesson.m_strUrl = strPage;
	else
		m_dlgLesson.m_strUrl = g_strAndesDir + g_szLessonDir + "\\" + strPage;

    int result = m_dlgLesson.DoModal();
    if (result == -1 || result == IDABORT) // failed to create dialog box
    {
    	DoWarningMessage(
    		"Couldn't open internal browser to show a Physics Concepts mini-lesson.\n\
    		Andes conceptual help requires Microsoft Internet Explorer 3.0 \n\
    		or later to be installed on the local machine.\n\
    		Andes will attempt to display lesson in an external web browser.");
    	// set flag to not show message again? For now, keep bugging them.

		// try to ShellExec the URL to show in default browser
		HINSTANCE hInst = ShellExecute(NULL, "open", strPage, NULL, NULL, SW_SHOWNORMAL); 
 		if ((int) hInst <= 32) {
 			TRACE("Shellexec failed, code = %d\n", (int) hInst);
 		}
		return; // early exit
    }
    
	// get here => Finished viewing modal minilesson
    Logf("Close-lesson");
}

// Open browser on a particular page.
// File argument treated as in ShowLesson.
// Differs from ShowLesson in that ShowLesson runs lesson as a modal dialog.
// This just opens the browser modelessly on the page so user can continue chat.
void CFBDApp::OpenBrowser(LPCTSTR pszName)
{
	CString strPage = pszName ? pszName : szDefaultPage;
	// If page name contains colon, assume it contains a full URL; 
	// else an ANDES-Review-relative file URL.
	CString strUrl;
	if (strPage.Find(':') != -1)
		strUrl = strPage;
	else
		strUrl = g_strAndesDir + g_szLessonDir + "\\" + strPage;
   
	// If browser is already up, just send it to new page. !! Problem if it
	// is currently up modally, helpsys should ensure this doesn't happen
	if (m_dlgLesson.m_hWnd != NULL) {
		// ensure it's visible, just to be safe.
		m_dlgLesson.ShowWindow(SW_SHOW);	
		m_dlgLesson.m_browser.Navigate(strUrl, NULL, NULL, NULL, NULL);
		return;
	}

	// else create modeless window
	m_dlgLesson.m_strUrl = strUrl;	// set browser to go here on init dialog
    if (! m_dlgLesson.Create()) {
		DoWarningMessage(
    		"Couldn't open internal browser to show a Physics Concepts mini-lesson.\n\
    		Andes conceptual help requires Microsoft Internet Explorer 3.0 \n\
    		or later to be installed on the local machine.\n\
    		Andes will attempt to display lesson in an external web browser.");
    	// set flag to not show message again? For now, keep bugging them.

		// try to ShellExec the URL to show in default browser
		HINSTANCE hInst = ShellExecute(NULL, "open", strPage, NULL, NULL, SW_SHOWNORMAL); 
 		if ((int) hInst <= 32) {
 			TRACE("Shellexec failed, code = %d\n", (int) hInst);
 		}
    } else {
		m_dlgLesson.ShowWindow(SW_SHOW);
	}
}

// Ensure browser window is closed. Safe to use even if not currently open
void CFBDApp::CloseBrowser()
{
	m_dlgLesson.DestroyWindow();
}
 
// menu command to open mini-lesson files. Temporary for testing mini-lesson appearance.
void CFBDApp::OnShowlesson() 
{
    // Prompt for filename. 
    CFileDialog dlg(TRUE, "htm", NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "Andes Lesson Page (.htm)|*.htm||");
    CString strDir = g_strAndesDir + g_szLessonDir;
    dlg.m_ofn.lpstrInitialDir = strDir;
    if (dlg.DoModal() != IDOK) 
    	return;
    // NB: ShowLesson does not take full pathname, just filename (relative to Andes Dir)
    ShowLesson(dlg.GetFileName());
	// Alt for testing:
	// OpenBrowser(dlg.GetFileName());
}

// Help menu commands that go to particular pages via ShowLesson:
void CFBDApp::OnHelpUnits() 
{
	ShowLesson("units.html");
}

// Help menu commands that go to particular pages via ShowLesson:
void CFBDApp::OnHelpConstants() 
{
	ShowLesson("constants.html");
}
// Show a video page, using local page if exists, else from web.
// strName is file basename, including extension, e.g. for1b.html
void CFBDApp::ShowVideoPage(CString strName)
{
	// Get the ShowLesson style path to prepend for a video page:
	// (Arg to showlesson is either full-URL, or Review-relative filename). 
	// default is to find videos on web site
	CString strPath = "http://www.andes.pitt.edu/Videos/" + strName;

	// Videos may be installed locally (CD version, or -- future -- add-in option)
	// Detect this by checking for the page file locally and adjust path
	CString strLocalContents = g_strAndesDir + "Review/Videos/" + strName;
	CFileStatus statFile;
	if (CFile::GetStatus(strLocalContents, statFile))  // exists locally
		strPath = "Videos/" + strName;

	// Show page in our minilesson browser (could open in user's browser)
	DoShowLesson(strPath);
}

// Help menu command to show index of all videos
void CFBDApp::OnHelpVideos() 
{
	ShowVideoPage("index.html");
}


    
// ShowRuleQuery : shows one of the rule query dialogs from the lesson dir.
void CFBDApp::ShowRuleQuery(LPCTSTR pszFileName)
{
    CRuleQDlg dlg;
    dlg.m_strPathName = g_strAndesDir + g_szLessonDir + "\\" + pszFileName; 
    dlg.DoModal();
}
    
void CFBDApp::OnShowruledlg() // menu command for testing
{
    // Prompt for filename. 
    CFileDialog dlg(TRUE, "htm", NULL, OFN_HIDEREADONLY | OFN_NOCHANGEDIR,
    	            "Rule dialog spec (.txt)|*.txt||");
    CString strDir = g_strAndesDir + g_szLessonDir;
    dlg.m_ofn.lpstrInitialDir = strDir;
    if (dlg.DoModal() != IDOK) 
    	return;
    // NB: ShowRuleQuery does not take full pathname, just filename
    ShowRuleQuery(dlg.GetFileName());
}
    
// ShowHint: show hint message w/followup buttons
// NB: This function is designed to be used as a DDE command handler --
// It expects the argument as formatted inside a DDE command string sent to the app
// from Lisp (no quotes). The hint dialog itself normally takes the quote-delimited 
// string values returned to us from help system RPC's.
void CFBDApp::ShowHintDdeCmd(LPCTSTR pszHintArg) 
{
#if 0 // No longer necessary, Lisp server was fixed
    // Hint dialog expects a quote-delimited string, since that is what it gets
    // when Allegro DDE returns a Lisp string value. However, it is evidently 
    // difficult for Lisp to send the "show-hint" command string to us with embedded
    // quotes around the hint spec argument (Allegro DDE formats strings as by Lisp
    // printer, which winds up prefixing backslashes to the embedded quotes). 
    // So, we accept the arg without delimiters and insert them here for consistency.
    CString strHintSpec;
    strHintSpec.Format("\"%s\"", pszHintArg);	// arg delimited with quotes
#endif  
	// send to worker routine in mainframe, which owns hint window
	GetMainFrame()->ShowHint(pszHintArg);
}
    
//
// Message boxes of various sorts
//	
void CFBDApp::DoInstructMessage(CString msg)
{
    	CInstructMsg msgBox;
    	msgBox.m_message = msg;
    	msgBox.DoModal();
}
    
int CFBDApp::DoWarningMessage(CString msg, CWnd* pParent, UINT nType)
{
		// need parent to handle msgs during startup (else MFC can set ownership to 
		// splash screen, causing assertion, other problems when it disappears).
    	CWarningMsg msgBox(NULL, pParent);
    	msgBox.m_message = msg;
    	msgBox.m_nType = nType ;
    	msgBox.m_bInfo = FALSE;
    	return msgBox.DoModal();
}
    
int CFBDApp::DoInfoMessage(CString msg, UINT nType)
{
    	CWarningMsg msgBox;
    	msgBox.m_message = msg;
    	msgBox.m_nType = nType;
    	msgBox.m_bInfo = TRUE;
    	return msgBox.DoModal();
}
    
/////////////////////////////////////////////////////////////////////////
// Options dialog boxes
// These should probably be made into pages of a property sheet.
/////////////////////////////////////////////////////////////////////////
    
//
// Feedback options:
//
void CFBDApp::OnHelpOptions() 
{
   	CHelpOptsDlg dlg;
   
   	dlg.m_bCheckEquations = m_bCheckEquations;
   	dlg.m_bCheckDiagram = m_bCheckDiagram;
   	dlg.m_nFeedback = m_nFeedback;
   	if (dlg.DoModal() == IDOK) {
   		m_bCheckEquations = dlg.m_bCheckEquations;
   		m_bCheckDiagram = dlg.m_bCheckDiagram;
   		m_nFeedback = dlg.m_nFeedback;
   	}	
}
    
//
// Example mode viewing options: 
//
void CFBDApp::OnViewOptions() 
{
   	CViewOptsDlg dlg;
   
   	dlg.m_nMaskMode = m_nMaskMode;
   	dlg.m_nGreyLevel = m_nGreyLevel;
   	if (dlg.DoModal() == IDOK) {
   		m_nMaskMode = dlg.m_nMaskMode;
   		m_nGreyLevel = dlg.m_nGreyLevel;
   	}
   	// Changing these parameters requires invalidating the view if it
   	// is open. Rather then find out, we just put another handler for
   	// this command in the view class, which has higher priority than
   	// this one in handling the command.
}
    
//
// For sending the app into log playback mode:
//
// The playback of log files is handled by starting a timer-controlled
// background task implemented in history.cpp. The user-interface for 
// interacting with the playback task is handled by the log player control 
// bar managed in the CMainFrame window. The set of user commands to
// manipulate the state of the playback task are currently handled in the
// Mainframe by the OnPlayer* functions. 
//
// So this function doesn't curretnly have much to do, it's only here because 
// we consider playback mode a global application state.
//
void CFBDApp::StartPlaybackMode(LPCTSTR pszPathName, IPlayerUI* pUI)
{
	// Global log player function will start
	// playback task from the beginning. If called re-entrantly
	// will just restart the log player with the specified filename.
	LogPlayerBegin(pszPathName, pUI);
}
    
    
////////////////////////////////////////////////////////////////////////////
//
// Following utility functions do the MFC magic to find principal 
// current objects (document, views and frames) from anywhere.
//
// Note that doc does not become "current" until after its MDI Child frame
// is activated. Because of that, these routines will not work while in the 
// middle of the doc/view creation sequence, e.g. in frame's OnCreateClient. 
//
////////////////////////////////////////////////////////////////////////////
    
// GetDocument: Generic MDI app routine to find currently active document.  
// Note return value is generic CDocument pointer.
CDocument* CFBDApp::GetActiveDoc()
{
	CWnd* pMainWnd = AfxGetMainWnd();
	if (!pMainWnd || !pMainWnd->IsKindOf(RUNTIME_CLASS(CMDIFrameWnd)))
		return NULL;
	CMDIFrameWnd* pMDIFrame =(CMDIFrameWnd*)pMainWnd;
	if (! pMDIFrame)
		return NULL;
   	CMDIChildWnd * pChild = pMDIFrame->MDIGetActive();
   	if ( !pChild)
   		return NULL;
	CDocument * pDoc = pChild->GetActiveDocument();
   	return (pDoc);
}

#ifdef _DEBUG
void DumpDoc()		// global; call from debugger to dump current doc to output window
{
	CDocument* pDoc = theApp.GetDocument();
	if (pDoc)
		pDoc->Dump(afxDump);
}
#endif _DEBUG 

// GetCurrentView: Generic MDI routine to find currently active view. 
// Optional view class filter can be specified; if non-NULL, active view must
// match specified class to succeed.
CView* CFBDApp::GetCurrentView(CRuntimeClass* pClass)
{
    CMDIFrameWnd* pMDIFrame =(CMDIFrameWnd*)AfxGetMainWnd();
	if (! pMDIFrame)
		return NULL;
   	CMDIChildWnd * pChild = pMDIFrame->MDIGetActive();
    if ( !pChild )
        return NULL;
    CView * pView = pChild->GetActiveView();
    if ( !pView || (pClass && ! pView->IsKindOf(pClass)) )
        return NULL;
    return  pView;
}
    
// FindView: Generic MDI routine to find first view of specified 
// class on current document, need not be active one
CView* CFBDApp::FindView(CRuntimeClass* pClass)
{
    CDocument* pDoc = GetDocument();
    if (pDoc == NULL)
    	return NULL;
    // search through list of views on document
    POSITION pos = pDoc->GetFirstViewPosition();
    while (pos != NULL) {
    	CView* pView = pDoc->GetNextView(pos);
    	if (pView->IsKindOf(pClass))
    		return  pView;
    }   
   	return NULL;
}
    
// Routines specific to our application:

// the routine formerly known as GetDocument (alias in header for backwards compatibility)
CFBDDoc* CFBDApp::GetCurrentProblem() // gets active problem/example document
{
	// First do quick check if have active doc and its a problem
	CDocument* pDoc = GetActiveDoc();
	if (pDoc && pDoc->IsKindOf(RUNTIME_CLASS(CFBDDoc))) 
		return (CFBDDoc*) pDoc;
	
	// Currently possible to make problem set the active doc while problem is open. 
	// So need to check if there is a problem open for working.
	// Search the problem doc template list for open problem document
	// Documents opened for other purposes (e.g. preview) should not be opened via
	// doc template so shouldn't be on this list.
	POSITION pos = m_ptmplProblem->GetFirstDocPosition();
	if (pos != NULL) {		// should be only one problem open
		pDoc = m_ptmplProblem->GetNextDoc(pos);
		if (pDoc) {
			ASSERT_KINDOF(CFBDDoc, pDoc);
			return (CFBDDoc*) pDoc;
		}
	}

	return NULL;
}

CProblemSet* CFBDApp::GetProblemSet() // get current problem set (need not be active)
{
	CDocument* pDoc = NULL;
	// look on problem set template's open document list
	POSITION pos = m_ptmplProbSet->GetFirstDocPosition();
	// should be only one problem set open
	if (pos != NULL) {
		pDoc = m_ptmplProbSet->GetNextDoc(pos);
		if (pDoc) ASSERT_KINDOF(CProblemSet, pDoc);
	}

	return (CProblemSet*) pDoc;
}

CProbSetView* CFBDApp::GetProbSetView()
{
	// look on view list of current probset document
	CProblemSet* pDoc = GetProblemSet();
	if (! pDoc) return NULL;
	// should be first view on it
    POSITION pos = pDoc->GetFirstViewPosition();
    while (pos != NULL) {
    	CView* pView = pDoc->GetNextView(pos);
    	if (pView->IsKindOf(RUNTIME_CLASS(CProbSetView)))
    		return  (CProbSetView*) pView;
    }   
   	return NULL;
}

// Find the example view: need not be active view
CEXView* CFBDApp::GetEXView()
{
    return (CEXView*) FindView(RUNTIME_CLASS(CEXView));
}
    
// Find the diagram view: need not be active one
CFBDView* CFBDApp::GetFBDView()
{	
   	return (CFBDView*) FindView(RUNTIME_CLASS(CFBDView));
}
    
// Find the equation view: need not be active view
CEQView* CFBDApp::GetEQView()
{
	return (CEQView*) FindView(RUNTIME_CLASS(CEQView));
#if 0 // not used yet -- still a problem w/dialog placement	if dlg doesn't fit in view.
	// now may have two EQViews if split. Following code searches to ensure
	// we return topmost one, for use when positioning dialogs.
	CDocument* pDoc = GetDocument();
    if (pDoc == NULL) return NULL;

	CView* pTopFound = NULL; // highest view found so far
	int yMin;				 // lowest top coord = highest top edge found so far
    POSITION pos = pDoc->GetFirstViewPosition();
    while (pos != NULL) {
    	CView* pView = pDoc->GetNextView(pos);
		if (pView->IsKindOf(RUNTIME_CLASS(CEQView))) { // hit an EQ View

			CRect rcView;
			pView->GetWindowRect(rcView);
			if ((pTopFound == NULL) 		// this is first one found
				|| (rcView.top < yMin))	{	// or its top is higher than previous
				pTopFound = pView; 
				yMin = rcView.top;
			}
		}
    } 
	if (pTopFound) ASSERT_KINDOF(CEQView, pTopFound);

   	return (CEQView*) pTopFound;
#endif 0
}
    
// Find the plan view: need not be active view
CPlanView* CFBDApp::GetPlanView()
{	
    return (CPlanView*) FindView(RUNTIME_CLASS(CPlanView));
}
    
// Find the ex plan view: note might not be active view
CEXPlanVw* CFBDApp::GetEXPlanVw()
{	
    return (CEXPlanVw*) FindView(RUNTIME_CLASS(CEXPlanVw));
}
    
// Find the variable view: note might not be active view
CVarView* CFBDApp::GetVarView()
{	
    return (CVarView*) FindView(RUNTIME_CLASS(CVarView));
}

// Find the hint view: note might not be active view
CHintView* CFBDApp::GetHintView()
{	
    return (CHintView*) FindView(RUNTIME_CLASS(CHintView));
}

// Find the interaction view: note might not be active view
CChatView* CFBDApp::GetChatView()
{
	return (CChatView*) FindView(RUNTIME_CLASS(CChatView));
}

// Find the hint view: note might not be active view
CTabView* CFBDApp::GetTabView()
{	
    return (CTabView*) FindView(RUNTIME_CLASS(CTabView));
}

//Find the HiLevelPlan view: note might not be active view
CHiLevelVw* CFBDApp::GetHiLevelVw()
{
    return (CHiLevelVw*) FindView(RUNTIME_CLASS(CHiLevelVw));
}

//Find the Principles view: note might not be active view
CPrincView* CFBDApp::GetPrincView()
{
    return (CPrincView*) FindView(RUNTIME_CLASS(CPrincView));
}
 
// Find the MDI Child frame wrapping the current document
CChildFrame* CFBDApp::GetChildFrame()
{
   	CMDIFrameWnd* pMDIFrame =(CMDIFrameWnd*)AfxGetMainWnd();
	if (! pMDIFrame)
		return NULL;
   	CMDIChildWnd * pChild = pMDIFrame->MDIGetActive();
	ASSERT_KINDOF(CChildFrame, pChild);
   	return (CChildFrame*)pChild;
}

//
// FBD App message filtering:
//
// This is called by the main CWinApp message loop (in CWinThread::Run) to filter
// every message before processing. The base class version will pass the message
// through a "filter chain" consisting of the PreTranslateMessage methods of all
// windows from the message target up through its parents to the main window, to
// see if any of them wish to filter it, before dispatching it for processing.
// This enables MFC CWnd's to hook their filters into the main message loop.
// MFC relies on this for forwarding mouse events to tooltips (in CWnd::PreTranslateMessage),
// kbd accelerator translation (in CFrameWnd's) and the keyboard
// interface in modeless dialogs (IsDialogMsg called in CDialog's).
//
// Our custom filtering first forwards msgs for filtering to splash screen, to enable 
// user to dismiss splash screen with a key or mouse click anywhere (Added by CG, 
// really not important feature for us).
//
// More important, implements an "ignore user input" mode by eating all messages 
// without processing. We use this mode during log playback to prevent user from
// interfering with the interface while retaining normal enabled appearance.
//
// Note that a standard non-MFC modal message pumping loop will bypass the 
// MFC PreTranslate filter mechanism. Use CWinThread::PumpMessage or call 
// it explicitly if it is important to ensure msg filtering is done in a modal loop.
//   
BOOL CFBDApp::PreTranslateMessage(MSG* pMsg)
{
//	if (pMsg->message == WM_EVENT_MSG)
//		Logf("EVENT_MSG");

	// Give splash screen first crack at message to see if should dismiss -- AW
   	// CG: The following line was added by the Splash Screen component.
   	if (CSplashWnd::PreTranslateAppMessage(pMsg))
		return TRUE;

	// Log mode filtering: ignore input events w/o disabling window:

   	// When running from log, ignore all user input from mouse and keyboard to 
	// anything other than the log player control. Don't want to disable the mainframe 
	// because that changes the app's appearance in some ways, e.g. can't set focus to 
	// an edit control and see caret.
	if (IgnoreInput()) 	// ignoring input (playing log, or remote commands).
	{
		// Let messages for the player control dlg be processed:
		// Player is a modeless dialog which normally filters messages via its own
		// PreTranslateMsg hook, called from our base class CWinThread::
		// PreTranslateMessage which walks the window hierarchy from msg target 
		// window up to mainframe, calling PreTranslateMsg hooks. The Dialog calls
		// IsDialogMsg to process keyboard and other events for the modal dialog.
		// Here check if its a message for dialog to translate.
		CMainFrame* pFrame = theApp.GetMainFrame();
		CDialog* pCtrlDlg = pFrame ? pFrame->GetPlayerControl() : NULL;
		HWND hwndCtrl = pCtrlDlg->GetSafeHwnd();
		if (hwndCtrl && pCtrlDlg->PreTranslateMessage(pMsg) )
			return TRUE;	// consumed by dialog's filter
		
		// Freeze rest of interface, including non-client area, by ignoring
		// all user input events for rest of app w/o processing:
		// Msg could still be non-client mouse message for player control,
		// so we also test hwnd of target (could do above).
		// Might also like to ignore MOUSEACTIVATE generated by windows on mouse clicks, 
		// so user clicks don't change window/view activation. View activation is not
		// explicitly logged since doesn't matter much. Still, active view affects MFC 
		// cmd route, and so which commands are enabled; also selection highlighting.
		// But can't easily do that here, since WM_MOUSEACTIVATE is *sent* to wndproc, 
		// not posted to msg queue. Would have to change view handlers to ignore it. 
		if (pMsg->hwnd != hwndCtrl) // msg not for player ctl
		{
			if ((pMsg->message >= WM_MOUSEFIRST && pMsg->message <= WM_MOUSELAST) 
			 || (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST) 
			 || (pMsg->message >= WM_NCMOUSEMOVE && pMsg->message <= WM_NCMBUTTONDBLCLK))		
			return TRUE;	// signal message consumed to ignore it
		}
		// else allow normal filtering. Don't fall through since don't want 
		// DDE wait mode filtering while in log playback
		return CWinApp::PreTranslateMessage(pMsg);
	}
	
	// DDE Wait mode filtering: ignore all input but Cancel Syskey == Alt-c
	CMainFrame* pFrame = theApp.GetMainFrame();
	if (pFrame && pFrame->InDdeWait()) 
	{
		if (pMsg->message == WM_SYSKEYDOWN && pMsg->wParam == 0x43) { // == Alt-c
			TRACE("Got DDE Cancel Syskey\n");
			HelpSystemCancelCall();
			return TRUE;
		}
		// additional filtering needed to prevent typing into focussed edit control
		if ((pMsg->message >= WM_MOUSEFIRST && pMsg->message <= WM_MOUSELAST) 
			 || (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST) 
			 || (pMsg->message >= WM_NCMOUSEMOVE && pMsg->message <= WM_NCMBUTTONDBLCLK))		
		return TRUE;	// signal message consumed to ignore it
	}

	// else do normal filtering:
	return CWinApp::PreTranslateMessage(pMsg);
}
    
    
void CFBDApp::ShowTCard(LPCTSTR pszName)
{
   	int idTCard = GetTCardID(pszName);
   	CFBDView* pView = GetFBDView();
   	if (pView!=NULL)
   		pView->ShowTCard(idTCard);
}
    
int CFBDApp::GetTCardID(CString name)
{
   	if (strcmp("body", name)==0)
   		return ID_HELP_DRAWBODY;
   	else if (strcmp("velocity", name)==0)
   		return ID_HELP_DRAWVELOCITY;
   	else if (strcmp ("force", name)==0)
   		return ID_HELP_DRAWFORCE;
   	else if (strcmp("acceleration", name)==0)
   		return ID_HELP_DRAWACCELERATION;
   	else if (strcmp("ruler", name)==0)
   		return ID_DRAWMOTIONBODY;
   	else if (strcmp("position", name) == 0)
   		return ID_DRAWPOSITION;
   	else if (strcmp("motion-velocity", name) == 0)
   		return ID_DRAWMOTVEL;
	else if (strcmp("displacement", name) == 0)
		return ID_HELP_DRAWDISPLACEMENT;
   	else 
   		return ID_DRAWMOTACC;
}
    
    
    

int CFBDApp::DoMessageBox(LPCTSTR lpszPrompt, UINT nType, UINT nIDPrompt) 
{
	if ((nIDPrompt == AFX_IDP_ASK_TO_SAVE) && !m_bAuthorMode){
		TRACE("asked to save\n");
		char name[255];

		sscanf(lpszPrompt, "%*s %*s %*s %s", name);
		CString msg;
		msg.Format(IDS_ASK_SAVECOPY, name);
		return CWinApp::DoMessageBox(msg, nType, nIDPrompt);
	}
	
	return CWinApp::DoMessageBox(lpszPrompt, nType, nIDPrompt);
}

//
// MakeSound: play sound for application event, if sounds on. 
//

// Data for event to sound mapping.
// Caches custom filename loaded as needed from registry profile
// Table is indexed by sound id, so order of entries must match enum order
static struct sndInfo {
	const char* pszRegName;			// entry name in registry profile 
	const char* pszDefaultFile;		// compiled in default, in case no reg entry
	const char*	pszFile;			// pts to filename to use, NULL if not set yet
} SoundTbl[] = {
// sndCorrectEntry,
	{ "Correct Entry", "media\\ding.wav", NULL },
// sndErrorEntry,
	{ "Error Entry", "media\\chord.wav", NULL},
// sndHintMsg,
	{ "Hint Msg", "media\\chimes.wav", NULL},
// sndWhatsWrongMsg,
	{ "Whats Wrong Msg", "media\\Utopia Question.wav", NULL },
// sndActivateApp,
	{ "Activate App", "media\\Musica Restore Up.wav", NULL},
// sndDeactivateApp,
	{ "Deactivate App", "media\\Musica Restore Down.wav", NULL},
};

void CFBDApp::MakeSound(EventSound sound)
{
	// keep quiet if sounds off (configurable in registry);
	if (! m_bPlaySounds) return;	

	// map event code to sound file via table
	if (int(sound) >= sndNUMSOUNDS) {
		TRACE("Playsound: Bad sound index %d\n", int(sound));
		// else just make default sound?
		return;
	}
	struct sndInfo* pSnd = &SoundTbl[sound];
	// check for custom filename from registry as needed, caching result in table
	if (pSnd->pszFile == NULL) 
	{
		CString strCustom = GetProfileString("Sounds", pSnd->pszRegName, "");
		// Empty string means use default. Can put garbage string in registry to turn off
		if (! strCustom.IsEmpty())
			pSnd->pszFile = _strdup(strCustom);	  // allocates copy, never freed.
		else
			pSnd->pszFile = pSnd->pszDefaultFile; // pts into default file entry in tbl
		
	}
	// Now play it:
	if (pSnd->pszFile) 
	{
		// Use ASYNC so sound comes out more nearly simultaneous with repaint showing feedback.
		// Use NOSTOP to not pre-empt a playing sound in progress -- in case where
		// have error feedback followed immediately by piggybacked hint msg, means
		// error sound will have priority in usual case of overlap.
		PlaySound(pSnd->pszFile, NULL, SND_FILENAME | SND_ASYNC | SND_NOSTOP);
	}
}

// CallTCardHelp: send command to training-card help instance.
// When command is HELP_CONTEXT (the default if unspecified), context id argument
// is taken as offset from base to be added in this routine.
void CFBDApp::CallTCardHelp(CWnd* pWnd, DWORD dwID, UINT nCmd /*= HELP_CONTEXT */)
{
	CString strPath = CString(m_pszHelpFilePath) + ">trainer";
	if (nCmd == HELP_CONTEXT)
		dwID += 0x00010000;
	::WinHelp(pWnd->GetSafeHwnd(), strPath, HELP_TCARD | nCmd, dwID);
}








