// 
// BrowsDlg.cpp : implementation file
// Embedded web browser dialog box.
// 

#include "stdafx.h"
#include "fbd.h"
#include "helpifc.h"
#include "history.h"
#include "mainfrm.h"	// for placement function MvDlgToBtmRight
#include "BrowsDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CBrowserDlg dialog


CBrowserDlg::CBrowserDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CBrowserDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CBrowserDlg)
	//}}AFX_DATA_INIT
	
/*	// default should be empty to indicate unset
	// following is default URL to open browser:
	m_strUrl = "http://www.pitt.edu/~vanlehn/andes.html";	// ANDES homepage
*/
	// flag when navigation is initiated by our program, (not user action)
	// used in processing the BeforeNavigate the browser sends us
	m_bOurNavigate = FALSE;

	// NULL position rect means unset.
	m_rcScreenPos.SetRectEmpty();
}


void CBrowserDlg::DoDataExchange(CDataExchange* pDX)
{
	CLogDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CBrowserDlg)
	DDX_Control(pDX, IDCANCEL, m_btnClose);
	DDX_Control(pDX, IDC_TOP_MARKER, m_stcTopMarker);
	DDX_Control(pDX, IDC_EDIT_ITEM, m_editTopic);
	DDX_Control(pDX, IDC_FORWARD, m_btnForward);
	DDX_Control(pDX, IDC_BACK, m_btnBack);
	DDX_Control(pDX, IDC_WEB_BROWSER, m_browser);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CBrowserDlg, CLogDialog)
	//{{AFX_MSG_MAP(CBrowserDlg)
	ON_BN_CLICKED(IDC_BACK, OnBack)
	ON_BN_CLICKED(IDC_FORWARD, OnForward)
	ON_WM_SIZE()
	ON_WM_MOVE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CBrowserDlg message handlers

BOOL CBrowserDlg::OnInitDialog() 
{
	// Base class method initializes dialog controls, and does the time-consuming work of 
	// trying to load the MS WebBrowser OLE control.
	// ???What happens on failure to load WebBrowser? Does exception throw out of here?
	CLogDialog::OnInitDialog();
		
	// Lock the WebBrowser control in memory on behalf of the app, to ensure
	// better performance on subsequent uses of the dialog.
	// !!! we never unlock it -- is that a problem? Presumably DLL must be
	// unloaded when workbench exits.
	static bLockedControl = FALSE;		// only lock it once.
	if (! bLockedControl) 
		bLockedControl = AfxOleLockControl(m_browser.GetClsid());
			
	// Trace msg on opening of mini-lesson viewer dialog: 
	LogEventf(EV_BROWSER_OPEN, m_strUrl);

	// restore previous browser position if saved,
	// else try bottom right default so as not to obscure hint window.
	if (! m_rcScreenPos.IsRectNull() ) {
		// MoveWindow(m_rcScreenPos);
		SetWindowPlacement(&m_wp);
	} else
		theApp.GetMainFrame()->MoveDlgToBtmRight(this);

	// send browser to URL. If unset. will open on browser's default.
	if (!m_strUrl.IsEmpty()) {
		m_bOurNavigate = TRUE;
		m_browser.Navigate(m_strUrl, NULL, NULL, NULL, NULL);
		m_bOurNavigate = FALSE;
	}

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// Control notification: User pushed back button on our dialog
void CBrowserDlg::OnBack() 
{
	LogEventf(EV_BROWSER_BACK, "");	// trace message, button event is logged
	// !!! flag is useless  since GoBack returns before the BeforeNavigate callback
	// Is this new behavior w/IE4.0? Affects log msg, which distinguishes user-initiated
	// navigations from trace of program-initiated ones. Seems harmless, since back button is
	// user-initiated, but means gets played back twice.
	m_bOurNavigate = TRUE;
	m_browser.GoBack();
	m_bOurNavigate = FALSE;	
}

// Control notification: User pushed forward button on our dialog
void CBrowserDlg::OnForward() 
{
	LogEventf(EV_BROWSER_FORWARD, "");	// trace message, button event is logged
	// !!! flag is useless  since GoBack returns before the BeforeNavigate callback
	// Is this new behavior w/IE4.0? Affects log msg, which distinguishes user-initiated
	// navigations from trace of program-initiated ones. Seems harmless, since back button is
	// user-initiated, but means gets played back twice.
	m_bOurNavigate = TRUE;
	m_browser.GoForward();
	m_bOurNavigate = FALSE;
}


//
// Notifications from embedded browser that we handle:
//
BEGIN_EVENTSINK_MAP(CBrowserDlg, CLogDialog)
    //{{AFX_EVENTSINK_MAP(CBrowserDlg)
	ON_EVENT(CBrowserDlg, IDC_WEB_BROWSER, 113 /* TitleChange */, OnTitleChangeWebBrowser, VTS_BSTR)
	ON_EVENT(CBrowserDlg, IDC_WEB_BROWSER, 105 /* CommandStateChange */, OnCommandStateChangeWebBrowser, VTS_I4 VTS_BOOL)
	ON_EVENT(CBrowserDlg, IDC_WEB_BROWSER, 100 /* BeforeNavigate */, OnBeforeNavigateWebBrowser, VTS_BSTR VTS_I4 VTS_BSTR VTS_PVARIANT VTS_BSTR VTS_PBOOL)
	ON_EVENT(CBrowserDlg, IDC_WEB_BROWSER, 102 /* StatusTextChange */, OnStatusTextChangeWebBrowser, VTS_BSTR)
	//}}AFX_EVENTSINK_MAP
END_EVENTSINK_MAP()

//
// On Browser Notification: Page title text has changed.
// 
void CBrowserDlg::OnTitleChangeWebBrowser(LPCTSTR Text) 
{
	m_editTopic.SetWindowText(Text);
}

//
// On Browser Notification: command enabled/disabled state has changed:
// 
void CBrowserDlg::OnCommandStateChangeWebBrowser(long Command, BOOL Enable) 
{
	// From ActiveX SDK docs on Microsoft site:
	const int CSC_NAVIGATEFORWARD = 0x01;
	const int CSC_NAVIGATEBACK = 0x02;
	// assume these are constants, not bit flags or'ed together
	if (Command == CSC_NAVIGATEFORWARD)
		m_btnForward.EnableWindow(Enable);
	if (Command == CSC_NAVIGATEBACK)
		m_btnBack.EnableWindow(Enable);
}

//
// On Browser Notification: Status bar text has changed
//
void CBrowserDlg::OnStatusTextChangeWebBrowser(LPCTSTR Text) 
{
/* Doesn't look that good where it is.
	// Put it in the message static (now misnamed).
	GetDlgItem(IDC_TASK_TEXT)->SetWindowText(Text);
*/
}


// Special URL's with this prefix mean: query help system for actual lesson to go to
static LPCTSTR szQueryPrefix = "HELP-";
static LPCTSTR szErrorPage = "Error.htm";

// 
// On Browswer Notification: browser is attempting to navigate somewhere.
//
void CBrowserDlg::OnBeforeNavigateWebBrowser(LPCTSTR URL, long Flags, LPCTSTR TargetFrameName, VARIANT FAR* PostData, LPCTSTR Headers, BOOL FAR* Cancel) 
{
	TRACE("%s browse to %s\n", m_bOurNavigate ? "Program" : "User", URL);

	// update URL member to track browser location
	// For experiment: page may contain link to .wmv video to be shown in externally in
	// Media Player. Don't update saved URL for this, because don't want to reopen video
	// after browser window is closed and re-opened (could never get back to contents!).
	LPCTSTR pLastDot = strrchr(URL, '.');
	if (! (pLastDot && (_stricmp(pLastDot, ".wmv") == 0))) {
		m_strUrl = URL;
	}
	
	// Log user operation if not program initiated. !!! Not correct anymore for 4.0
	if (! m_bOurNavigate)
		LogEventf(EV_BROWSER_NAVIGATE, URL);
	else // following really just trace info, also backwards compatible w/old logs
		LogEventf(EV_BROWSER_GO, URL);


	// See if link is for start or end of post-problem KCD. If so, update KCD state
	// Note start event is program-initiated, so do before test below.
	// Allow disabling by registry entry, for non-implemented KCDs
	
		CFBDDoc* pDoc = theApp.GetCurrentProblem();
		if (m_strUrl.Find("navalkcds") != -1 && pDoc) {
			pDoc->m_fKcdState |= KCD_STARTED;
		}
		if (m_strUrl.Find("completed") != -1 && pDoc) {
			pDoc->m_fKcdState |= KCD_COMPLETED;
		}

	// if our code programmatically sent the browser here, do nothing more. [needed?]
	if (m_bOurNavigate) 
		return;

	// see if it's an aps link click to launch a problem set
	if (pLastDot && (_stricmp(pLastDot, ".aps") == 0)) {
		// set flag to end the dialog
		EndDialog(IDOK);
		*Cancel = TRUE;
		return;
	}




#if 0 // helpsys-computed links no longer used	

	// Extract file title = basename w/o extension.
	LPCTSTR pLastSlash = strrchr(URL, '\\');
	LPCTSTR pStart = pLastSlash ? pLastSlash + 1 : URL;
	LPCTSTR pPastEnd = pLastDot ? pLastDot : &URL[strlen(URL)];
	int nLen = pPastEnd - pStart;
	char szTitle[255];
	strncpy(szTitle, pStart, nLen);
	szTitle[nLen] = '\0';
	
	// see if it is our special one
	if (_strnicmp(szTitle, szQueryPrefix, sizeof(szQueryPrefix)) == 0) // case-insensitive
	{
		// Notify help system of request to navigate
		LPCTSTR pszResult = HelpSystemExecf("(Navigate-to |%s|)", szTitle);
		
		// (commented code allowed return of T to means help system doesn't care
		if (!pszResult /* failed */ /* || strcmp(pszResult, "T") == 0 */) 
		{
			// If help system is down, nowhere to go -- should build an error page 
			*Cancel = TRUE;
			return;
		}

		// else Help system should return name of file to navigate to
		// May need to extract it from surrounding quotes
		CString strRawResult(pszResult);
		CString strFileName;
	//	if (strRawResult[0] == '\"')	// strip quotes added by Lisp
	//		strFileName = strRawResult.Mid(1, strRawResult.GetLength() - 2);
	//	else
		strFileName = strRawResult;

		// result should be relative pathname but including extension
		m_strUrl = g_strAndesDir + g_szLessonDir + "\\" + strFileName;

		// Want to send browser to a different location. But that will generate a new
		// notification, which must be ignored.
		m_bOurNavigate = TRUE;
		m_browser.Navigate(m_strUrl, NULL, NULL, NULL, NULL);
		m_bOurNavigate = FALSE;
		// if not, need to post message to ourselves to navigate

		// Cancel navigate via out parm
		*Cancel = TRUE;
	}
#endif 0
}

// Window event: dialog has been resized by user:
void CBrowserDlg::OnSize(UINT nType, int cx, int cy) 
{
	CLogDialog::OnSize(nType, cx, cy);
	
	// if its been created, resize embedded WebBrowser control to fit.
	// (Must make sure because we get WM_SIZE messages dureing creation.)
	if (m_browser.GetSafeHwnd() != NULL) 
	{
		/* Get weird sizing failures with some old versions (original 3.0?).
		   Try avoiding control-specific methods with generic CWnd methods.
		   These translate to OLE interface calls for OLE control CWnds.
		m_browser.SetWidth(cx);
		m_browser.SetHeight(cy - m_browser.GetTop()); */
		CRect rcCur;
		m_browser.GetWindowRect(rcCur);
		ScreenToClient(rcCur);
		int cyNew = cy - rcCur.top;

		m_browser.SetWindowPos(NULL, 0, 0, cx, cyNew, SWP_NOZORDER | SWP_NOMOVE);

		// save the position 
		GetWindowRect(m_rcScreenPos);
		// could use this instead -- can find out if maximized
		m_wp.length = sizeof (WINDOWPLACEMENT);
		GetWindowPlacement (&m_wp);
	}
}

// Window event: dialog has been moved by user
void CBrowserDlg::OnMove(int x, int y) 
{
	CLogDialog::OnMove(x, y);
	
	// save the new position, ignoring size messages during create 
	if (m_browser.GetSafeHwnd() != NULL) {
		GetWindowRect(m_rcScreenPos);
		// could use this instead -- can find out if maximized
		m_wp.length = sizeof (WINDOWPLACEMENT);
		GetWindowPlacement (&m_wp);
	}
}

void CBrowserDlg::OnOK()  // eat the Enter key without closing window 
{
	TRACE("Browser received OK - ignoring\n");
}

// Replay log events. Generic dialog/ctl events may be passed to CLogDialog handler
// Note generic button event handler in base class may process "BACK" and "FORWARD" 
// Button click events.
BOOL CBrowserDlg::DispatchEvent(EventID id, LPCTSTR pszArgs)
{
	switch (id) 
	{
	case EV_BROWSER_CLOSE: // logged after close. Can happen via close button, or Esc.
		OnCancel();
		return TRUE;

	case EV_BROWSER_GO:
		// Replay "program-initiated" changes also, since: 
		// (1) just to be on safe side (should be harmless).
		// (2) Needed anyway to replay the OPEN TEXTBOOK toolbar cmd, since that opens browser on its internally saved
		// last URL, which might be different in the replay context if several logs played.
	case EV_BROWSER_NAVIGATE:
		// replay user-initiated navigation: send browser to dest. Don't set 
		// m_bOurNavigate so notification is sent and its handling is replayed.
		m_browser.Navigate(pszArgs, NULL, NULL, NULL, NULL);
		return TRUE;

	case EV_BTN_CLICK:
		// Consume button click events for FORWARD/BACK (the only buttons of interest --
		// the CLOSE event is logged outside of dialog.) and just replay the 
		// navigate events. Replaying *both* forward/back and navigates caused errors
		// in FF log play.
		return TRUE;

	default:
		break;
	}
	// else pass to base class (mainly for dialog moves, now).
	return CLogDialog::DispatchEvent(id, pszArgs);
}

