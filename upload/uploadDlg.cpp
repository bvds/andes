// uploadDlg.cpp : implementation file
//

#include "stdafx.h"
#include "upload.h"
#include "uploadDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CUploadDlg dialog

// String constants for FBD registry entries
const char* szFBDAppName = "FBD";
const char* szSettingsSection = "Settings";
const char* szInstallDirKey = "Install Directory";
const char* szDefaultAndesPath = "C:\\Program Files\\Andes\\";

// Constants for files of interest:
const char* szLogDirName = "Log";
const char* szLogExt = ".log";
const char* szLogWildcard = "*.log";
const char* szStudentDirName = "Students";

#if 0 // unused
static char szComputer[MAX_COMPUTERNAME_LENGTH + 1] = "Unknown";
static char szWinUser[MAX_COMPUTERNAME_LENGTH + 1] = "User"; // windows user name, not ours
#endif 0

CUploadDlg::CUploadDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CUploadDlg::IDD, pParent)
{
	
	//{{AFX_DATA_INIT(CFTPDlg)
	m_strPassword = _T("");
	m_strHost = _T("");
	m_strUserName = _T("");
	//}}AFX_DATA_INIT

	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	// MFC bug: CWinApp::LoadIcon uses old Win32 ::LoadIcon which only loads
	// 32 x 32 size image; this is then shrunk to fit in title-bar.
	// Workaround loads additional small icon to register below.
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);	// loads large image only
	m_hIconSmall = (HICON)::LoadImage(AfxGetResourceHandle(),
								MAKEINTRESOURCE(IDR_MAINFRAME), IMAGE_ICON,
								16, 16, 0);
	// Init overall sucess/failure flag
	m_bSucceeded = FALSE;
	// "global" abort flag 
	m_bQuit = FALSE;

	// dialog is created fullsize (expanded); contracted in OnInitDlg
	m_bExpanded = TRUE;

	// Init main config info from registry:

	// Load Pathname of Andes Installation from Registry.
	// Note we want to access FBD app's profile data, not UPLOAD's!
	// In case SetRegistryKey has been used, MFC loads profile data from
	// registry section in m_pszProfileName, which is set to the application name.
	// We temporarily change App's m_pszProfileName around profile reading call
	// to get MFC to access the FBD profile. Note this requires that SetRegistryKey 
	// has been called earlier, and that UPLOAD profile is under the same registry key as FBD's.
	CWinApp* pApp = AfxGetApp();
	LPCTSTR pszOldName = pApp->m_pszProfileName;
	pApp->m_pszProfileName = szFBDAppName;

	// Load path of Andes directory. ("path" means can concatenate w/filenames).
	m_strAndesPath = pApp->GetProfileString(szSettingsSection, szInstallDirKey, "");
	// If unset, use EXE's directory as default. 
	if (m_strAndesPath.IsEmpty()) 
	{
		TCHAR szPathBuf[_MAX_PATH];
		VERIFY(::GetModuleFileName(pApp->m_hInstance, szPathBuf, _MAX_PATH));
		char * pLastBackslash = strrchr(szPathBuf, '\\'); // !!assumes never "C:\fbd.exe"
		if (pLastBackslash != NULL) {
			pLastBackslash[1]  = '\0';	// clobbers ch in buf after dir end
			m_strAndesPath = szPathBuf;
		}
	}
	// Ensure it ends in backslash so can just prefix it to filenames
   	if (!m_strAndesPath.IsEmpty() && m_strAndesPath.Right(1) != "\\" )
   		m_strAndesPath += "\\";
    
	// restore back to our (UPLOAD) profile in the Andes Group section of registry.
	pApp->m_pszProfileName = pszOldName;

	// Derive Log file and Student file directory names
	m_strLogDir = m_strAndesPath + szLogDirName;
	m_strStudentDir = m_strAndesPath + szStudentDirName;

#if 0 // now unused
	// save some identifying info about the machine and user.
	// Note Windows login name is not nec. Andes login handle nor ftp server userid.
	unsigned long nSize = MAX_COMPUTERNAME_LENGTH + 1;
	::GetComputerName(szComputer, &nSize);
	nSize = MAX_COMPUTERNAME_LENGTH + 1;	// must re-init, it's in-out above
	::GetUserName(szWinUser, &nSize);

	// copy into CStrings for convenience.
	m_strComputer = szComputer;
	m_strWinUser = szWinUser;
#endif 0
}

void CUploadDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CUploadDlg)
	DDX_Control(pDX, IDC_ACCOUNT_PROMPT, m_stcAcctPrompt);
	DDX_Control(pDX, IDC_M_ALPHA, m_stcMidPrompt);
	DDX_Control(pDX, IDC_PASSWORD, m_editPassword);
	DDX_Control(pDX, IDC_PLACEHOLDER, m_stcExArea);
	DDX_Control(pDX, IDC_STATUS_MSG, m_textStatusMsg);
	DDX_Control(pDX, IDC_USERNAME, m_editUserName);
	DDX_Control(pDX, IDC_MESSAGE_TEXT, m_textMsg);
	DDX_Control(pDX, IDC_SERVER, m_cboHosts);
	DDX_Text(pDX, IDC_PASSWORD, m_strPassword);
	DDX_CBString(pDX, IDC_SERVER, m_strHost);
	DDX_Text(pDX, IDC_USERNAME, m_strUserName);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CUploadDlg, CDialog)
	//{{AFX_MSG_MAP(CUploadDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_ENTER_DATA, OnEnterData)
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CUploadDlg message handlers


// registry info for Upload app
const char* szUploadSection = "Account Info";	
const char* szUserKey = "User ID";		// value id for saved user name
const char* szHostKey = "Server";		// value id for saved machine name
const char* szUpDirKey = "Directory";	// value id for main upload dir on server

#if 0  // BvdS:  This must be dead code
// constants for USNA environment:
#ifndef PITT
const char* szBaltic = "baltic.nadn.navy.mil";
const char* szCoral = "coral.nadn.navy.mil";
const char* szArctic = "arctic.nadn.navy.mil";
const char* szNavyDir = "/file.exchange/mids/Andes/data";
// for testing locally (presence of this choice a little non-obvious in combo box).
 const char* szAndesServer = "unix.cis.pitt.edu";
const char* szAndesDir = "upload";
#else // PITT version
const char* szAndesServer = "unix.cis.pitt.edu";
const char* szAndesDir = "upload";
const char* szAndesUser = "andes2";
const char* szNavyDir = ".";		// shouldn't be used, but code references.
#endif 
#endif
BOOL CUploadDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	// Added workaround for MFC icon loading bug to set small-size icon as well.
	SetIcon(m_hIcon, TRUE);				// Sets big icon
	SetIcon(m_hIconSmall, FALSE);		// Sets small icon
	
	// TODO: Add extra initialization here

	// fill combo with hosts to choose from. Most likely one should be first.
	m_cboHosts.ResetContent();

#ifndef PITT
	m_cboHosts.AddString(szBaltic);	// 2000: most sophomores use this
	m_cboHosts.AddString(szCoral);	// 1999: most sophomores used this
	m_cboHosts.AddString(szArctic);
	m_cboHosts.AddString(szAndesServer);	// for our local testing
	// set default selection in the combo box. First should be the most common case.
	m_cboHosts.SetCurSel(0);

	// See if we can init some  parms from earlier sesssions in the registry, else init to defaults:
	CString strInitUserName = AfxGetApp()->GetProfileString(szUploadSection, szUserKey, "");
	if (!strInitUserName.IsEmpty())
		m_editUserName.SetWindowText(strInitUserName);
	else
		m_editUserName.SetWindowText("m??????" /* m_strWinUser */);
	
	// Set host string if set from registry
	CString strInitHost = AfxGetApp()->GetProfileString(szUploadSection, szHostKey, "");
	if (!strInitHost.IsEmpty())
		m_cboHosts.SetWindowText(strInitHost);

#else //PITT
	// all input fields disabled. Default values displayed to avoid confusion
	m_cboHosts.AddString("Andes Server");
	m_cboHosts.SetCurSel(0);
	m_cboHosts.EnableWindow(FALSE);
	m_editUserName.SetWindowText("Andes Student");
	m_editUserName.EnableWindow(FALSE);
	m_editPassword.SetWindowText("******");
	m_editPassword.EnableWindow(FALSE);
	// don't show mids m + alpha prompt at pitt
	m_stcMidPrompt.ShowWindow(SW_HIDE);
	m_stcAcctPrompt.EnableWindow(FALSE);
#endif 
	
	// dialog comes up in contracted mode -- expands during transfer
	ContractDlg();
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CUploadDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CUploadDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CUploadDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

// User hit the enter button after typing user name and password
void CUploadDlg::OnEnterData() 
{
#ifndef PITT
	// get the parms out of the controls
	UpdateData(TRUE);

	// make sure we have what we need to go on
	if (m_strHost.IsEmpty() ||
		m_strUserName.IsEmpty() ||
		m_strPassword.IsEmpty()) {
		AfxMessageBox("Not all Account Info fields filled in");
		return;
	}
	// Since parms entered, save in profile so user doesn't have to type it next time.
	AfxGetApp()->WriteProfileString(szUploadSection, szUserKey, m_strUserName);
	AfxGetApp()->WriteProfileString(szUploadSection, szHostKey, m_strHost);

#else
	// all data hardcoded
	m_strHost = szAndesServer;
	m_strUserName = szAndesUser;
	m_strPassword = m_strUserName;
#endif

	// Now do all the work
	DoTransfer();
}

void CUploadDlg::DoTransfer()
{
	// Figure out main upload directory we should go to.
	CString strDir;
	// first consult the registry (in case we have to change this later.)
	strDir = AfxGetApp()->GetProfileString(szUploadSection, szUpDirKey, "");
	// if nothing there, use hard-coded choice based on server chosen
	if (strDir.IsEmpty())  
	{
		if (m_strHost == szAndesServer)	
			strDir = szAndesDir;
		else 
			strDir = szNavyDir;
	}

	//  do the transfers using the Wininet functions //
	ExpandDlg();		// expanded dialog shows trace messages

	CString strMsg;
	static int nConAttempts = 0;
	int nLogs = 0; int nFailed = 0;
	int nStudentFiles = 0; int nStudentFailed = 0;
	
	// open the internet library
	HINTERNET hInet;
	HINTERNET hConnect = NULL;
	if ( !(hInet = InternetOpen ( "Andes Workbench",  INTERNET_OPEN_TYPE_PRECONFIG , NULL, 0, 0) ) )
    {
        Error(GetLastError(), "Couldn't initialize Internet library. ");
		// this is really bad error, probably unrecoverable
        goto exit_canquit;
    }

	// 
	// try to make the FTP connection
	//
	++nConAttempts;
	strMsg.Format("Attempting FTP connection to %s", m_strHost); AddLog(strMsg);
	{ CWaitCursor hourglass;	// shows hourglass cursor till end of block
	  hConnect = InternetConnect ( hInet, m_strHost, INTERNET_INVALID_PORT_NUMBER, 
								m_strUserName,  m_strPassword, INTERNET_SERVICE_FTP, /* INTERNET_FLAG_PASSIVE */ 0, 0);
	}
	if (! hConnect)
	{	
		Error (GetLastError(), "Couldn't connect to server. ");
		AfxMessageBox("Failed to connect to server. Check username and password and try again.");
		// show them quit button after several attempts
		if (nConAttempts >= 3)
			GetDlgItem(IDCANCEL)->ShowWindow(SW_SHOW);
		// exit handler to give them chance to try everything again
		InternetCloseHandle(hInet);
		// restore contracted mode
		ContractDlg();
		return;
	} else
		AddLog("Connected");
	
	
	// change to the designated upload directory on the server machine
	strMsg.Format("CD'ing to %s", strDir); AddLog(strMsg);	
	if (! FtpSetCurrentDirectory(hConnect, strDir) ) {
		 Error (GetLastError(), "Couldn't change to upload directory. ");
	} else
		AddLog("CD OK");

#ifndef PITT
	// try to create subdirectory for this student's logs. Create by username
	// for the server machine account. May fail if already exists but we barge on.
	strMsg.Format("Creating subdirectory %s",m_strUserName); AddLog(strMsg);
	if (! FtpCreateDirectory(hConnect, m_strUserName) ){
		Error(GetLastError(), "(May already exist).");
	} else 
		AddLog("MDKIR OK");

	// try to change into the student's upload directory indexed by server userid.
	strMsg.Format("CD'ing to %s", m_strUserName); AddLog(strMsg);
	if (! FtpSetCurrentDirectory(hConnect, m_strUserName) ){
		Error(GetLastError(), "CD failed ");
	} else 
		AddLog("CD OK");
#endif ! PITT

	// Now run loop to upload each log data file
	AddLog("Transferring session logs:");
	MPut(hConnect, m_strLogDir, szLogWildcard, nLogs, nFailed);
	strMsg.Format("Transferred %d of %d log files", nLogs - nFailed, nLogs); AddLog(strMsg);

	// Now run loop to upload Student data
	AddLog("Transferring student files");
/*Only one student history file in Andes2, and it's now in Log directory. 
	// MPut(hConnect, m_strStudentDir, "*.old", nStudentFiles, nStudentFailed);
	// MPut(hConnect, m_strStudentDir, "*.ctx", nStudentFiles, nStudentFailed);
	MPut(hConnect, m_strStudentDir, "*.*", nStudentFiles, nStudentFailed); */
	MPut(hConnect, m_strLogDir, "*.dat", nStudentFiles, nStudentFailed, /*bErrorIfNone:*/FALSE); 
	strMsg.Format("Transferred %d of %d student files", nStudentFiles - nStudentFailed, nStudentFiles);
	AddLog(strMsg);

	// if everything succeeded we are done
	m_bSucceeded = (!m_bQuit && (nFailed == 0) && (nStudentFailed == 0));

	// Sweet smell of success !
	if (m_bSucceeded) 
		GetDlgItem(IDC_ENTER_DATA)->EnableWindow(FALSE);	// not going to retry
	
	// clean up connection 
	InternetCloseHandle(hConnect);
	InternetCloseHandle(hInet);

exit_canquit:
	GetDlgItem(IDCANCEL)->ShowWindow(SW_SHOW);

	if (m_bSucceeded) 
	{
		AfxMessageBox("Data Upload completed successfully, Thank you. Press Quit to exit.");
		// EndDialog(IDOK);
	} 
	else 
	{
		// if get here, succeded in making connection, but user aborted or not all transfers worked.
		if ( m_bQuit )
			strMsg.Format("Upload cancelled after %d transfers and %d failures.\n",
			              (nLogs + nStudentFiles) - (nFailed + nStudentFailed),
						  (nFailed + nStudentFailed));
		else if ( (nFailed != 0) || (nStudentFailed != 0) )
			strMsg.Format("Upload incomplete: Transfer failed on %d of %d files.\n",
							nFailed + nStudentFailed, nLogs + nStudentFiles);
		else 
			strMsg.Format("An error occurred during upload attempt. ");
		strMsg += "Please try to complete at another time. Notify instructor if failures persist."; 
		AfxMessageBox(strMsg);
	}
}

void CUploadDlg::MPut(HINTERNET hConnect, CString& strDir, LPCTSTR szWildcard, int& nFiles, int& nFailed,
					  BOOL bErrorIfNone/*=TRUE*/)
{
	// nFiles = 0; nFailed = 0; // accumulators:  may be non-zero coming in	
	BOOL bFinished = FALSE;
	HANDLE hSearch;
	WIN32_FIND_DATA FileData;
	CString strMsg;
	BOOL bSucceeded;

	CString strSpec = strDir + "\\" + szWildcard; 
	hSearch = ::FindFirstFile(strSpec , &FileData);
	if (hSearch == INVALID_HANDLE_VALUE) {
		if (bErrorIfNone) {
			Error(GetLastError(), "No files found");
		}
		return;
	}
	
	while ( ! bFinished )
	{
		// get current filename
		m_strFileName = FileData.cFileName;
		
		// ignore special directory files matched with *.*
		if (m_strFileName.IsEmpty() || m_strFileName == "." || m_strFileName == "..")
			goto NextFile; // skips over transfer code

		// update count of files tried to transfer:
		++nFiles;

		// Get source pathname for the transfer.
		m_strPathName = strDir + "\\" + m_strFileName;
		
		strMsg.Format("Sending (%i): %s", nFiles, m_strPathName); // AddLog(strMsg); 
		m_textStatusMsg.SetWindowText(strMsg);

		// Do it:  dst name is just file name (will put it relative to current remote directory)
		// Note this call not interruptible. But can put escapes into message queue during it.
		{ CWaitCursor hourglass;
		   bSucceeded = FtpPutFile (hConnect, m_strPathName, m_strFileName, 
						              FTP_TRANSFER_TYPE_BINARY, 0 );
		}
		if (! bSucceeded)
		{
			strMsg.Format("Couldn't transfer %s", m_strPathName);
			Error (GetLastError(), strMsg);
			++nFailed;
		}
		if (!ProcessPendingEvents()) {
			AddLog("MPut cancelled");
			return;
		}

NextFile: // advance search to next file
		if (!::FindNextFile(hSearch, &FileData)) 
		{
			if (::GetLastError() == ERROR_NO_MORE_FILES) 
				bFinished = TRUE; 
			else 
				AddLog("Couldn't find next file!"); 
		}
	}

}

void CUploadDlg::Error(DWORD dwError, LPCTSTR pszMsg)
{
	CString strMsg; 
	strMsg.Format("ERROR (%d). %s ", dwError, pszMsg);
	
	if (dwError == ERROR_INTERNET_EXTENDED_ERROR) // we have extended info
	{
		DWORD  dwIntError;
		char szBuffer[255] = "";
		DWORD dwLength = 255;

		InternetGetLastResponseInfo (&dwIntError, szBuffer, &dwLength);
		strMsg += szBuffer;
	}

	AddLog(strMsg);
}


void CUploadDlg::AddLog(LPCTSTR pszMsg)
{
	m_strLog += pszMsg;
	m_strLog += "\r\n";
	m_textMsg.SetWindowText(m_strLog);
	m_textMsg.LineScroll(1);   // !!! really want to scroll so last line is visible 
	// m_textMsg.UpdateWindow();
	
}



// Poll for and dispatch any pending user inputs. 
// Sprinkle calls to this inside any time-consuming loop.
// Return True iff app should continue, i.e. m_bQuit not set by handler
BOOL CUploadDlg::ProcessPendingEvents()
{
	MSG msg;
	while (!m_bQuit && ::PeekMessage (&msg, NULL, 0, 0, PM_NOREMOVE))
	{
		// TranslateMessage(&msg);
		// DispatchMessage(&msg): 

		// From Prosise Printing AbortProc example:
		AfxGetThread()->PumpMessage(); 
	}
	return ! m_bQuit; 
}

void CUploadDlg::OnClose() 
{
	m_bQuit = TRUE;	
	CDialog::OnClose();
}

void CUploadDlg::OnCancel() 
{
	m_bQuit = TRUE;
	CDialog::OnCancel();
}

void CUploadDlg::ContractDlg()
{
	if (m_bExpanded) {
		CRect rcDlg, rcExArea;
		m_stcExArea.GetWindowRect(rcExArea);
		GetWindowRect(rcDlg);
		MoveWindow(rcDlg.left, rcDlg.top, rcDlg.Width(), rcDlg.Height() - rcExArea.Height());
		m_bExpanded = FALSE;
	}
}

void CUploadDlg::ExpandDlg()
{
	if (!m_bExpanded) {
		CRect rcDlg, rcExArea;
		m_stcExArea.GetWindowRect(rcExArea);
		GetWindowRect(rcDlg);
		MoveWindow(rcDlg.left, rcDlg.top, rcDlg.Width(), rcDlg.Height() + rcExArea.Height());
		m_bExpanded = TRUE;
	}
}


