// 
// PlayDlg.cpp : implementation file. Modeless dialog
// tool for controlling log file playback.
//
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "Mainfrm.h"		// for close message defines 
#include "PlayDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPlayDlg dialog


// custom constructor for modeless dlg, sets frame to notify on close
// That need not be parent!
CPlayDlg::CPlayDlg(CFrameWnd* pFrame) 
	: CDialog(CPlayDlg::IDD)	// 
{
	m_pFrame = pFrame; 
	//{{AFX_DATA_INIT(CPlayDlg)
	m_strFileName = _T("");
	m_nSecs = 0;
	m_nMins = 0;
	m_nHrs = 0;
	//}}AFX_DATA_INIT
}

// custom window creation function for this class
BOOL CPlayDlg::Create(CWnd* pParent /*=NULL*/)
{
	return CDialog::Create(CPlayDlg::IDD, pParent);
	
}

BOOL CPlayDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	UpdatePlayerUI();
	
	// TODO: Add extra initialization here

	// set slider to show percentage complete
	m_slider.SetRange(0, 100, FALSE);
	m_slider.SetTicFreq(10);		// tick marks every ten units
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CPlayDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPlayDlg)
	DDX_Control(pDX, IDC_MESSAGE2, m_txtPlayed);
	DDX_Control(pDX, IDC_PLAYER_SLIDER, m_slider);
	DDX_Control(pDX, IDC_LOGTIME_TEXT, m_txtTime);
	DDX_Control(pDX, IDC_MESSAGE, m_txtMessage);
	DDX_Text(pDX, IDC_LOGFILE_NAME, m_strFileName);
	DDX_Text(pDX, IDC_GOTO_SECONDS, m_nSecs);
	DDV_MinMaxUInt(pDX, m_nSecs, 0, 60);
	DDX_Text(pDX, IDC_GOTO_MINUTES, m_nMins);
	DDV_MinMaxUInt(pDX, m_nMins, 0, 60);
	DDX_Text(pDX, IDC_GOTO_HOURS, m_nHrs);
	DDV_MinMaxUInt(pDX, m_nHrs, 0, 99);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPlayDlg, CDialog)
	//{{AFX_MSG_MAP(CPlayDlg)
	ON_WM_CLOSE()
	ON_BN_CLICKED(IDC_SELECT_LOGFILE, OnSelectLogfile)
	ON_BN_CLICKED(ID_PLAYER_GOTO, OnPlayerGoto)
	ON_WM_DROPFILES()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

//
// Display callbacks from the player object: Note might be fired programmatically when
// dialog is not visible.
//
void CPlayDlg::UpdatePlayerUI()	// update ui to synch w/current player state
{
	if (GetSafeHwnd() == 0)		// Window is not up
		return;

	// this will update from command update handlers in the frame.
	UpdateDialogControls(m_pFrame, TRUE);

	// Enable browse button (= open log file command)
	// Following is simpler than putting in update command ui handler
	
	GetDlgItem(IDC_SELECT_LOGFILE)->EnableWindow(!LogPlayerInPlayback());
}

void CPlayDlg::ShowNextEvent(LPCTSTR szMsg)
{
	if (GetSafeHwnd() != 0)	{		// Dialog window is attached

		// move previous event into "played" window.
		CString strPrev;
		m_txtMessage.GetWindowText(strPrev);
		m_txtPlayed.SetWindowText(strPrev);

		m_txtMessage.SetWindowText(szMsg);
	}
}	

void CPlayDlg::SetPlaybackTime(LPCTSTR szTime)
{
	static CString strLastTime;
	if (GetSafeHwnd() != 0)	{		// Dialog window is attached
		if (strLastTime != szTime) {
			m_txtTime.SetWindowText(szTime);
			strLastTime = szTime;
		}
	}
}

void CPlayDlg::SetProgress(int nPercent)
{
	if (nPercent < 0) 
		nPercent = 0;
	else if (nPercent > 100)
		nPercent = 100;

	m_slider.SetPos(nPercent);
}

void CPlayDlg::NotifyFinished()
{
	if (GetSafeHwnd() != 0) 
		AfxMessageBox("Log Playback ended", MB_ICONINFORMATION| MB_OK);
}
/////////////////////////////////////////////////////////////////////////////
// CPlayDlg message handlers

//
// General modeless event handlers:
//

// We have no OK/Cancel buttons on the form. But must handle anyway to catch 
// events generated from dialog keyboard accelerators (Esc->IDCANCEL, Return->IDOK).
void CPlayDlg::OnCancel() 
{
	// first generate a stop command to end playback
	m_pFrame->SendMessage(WM_COMMAND, ID_PLAYER_STOP);

	// and signal mainframe to close modal tool
	// UpdateData(TRUE);	/* if have any data to DDX  here */
	if (m_pFrame != NULL)
		m_pFrame->PostMessage(WM_CLOSE_PLAY_DLG, IDCANCEL);
}

void CPlayDlg::OnOK()
{
	// Eat return without closing. 
	// User might have hit it out of habit while entering filename, e.g.
}

void CPlayDlg::OnClose() 
{
	// first generate a stop command to ensure playback has ended
	m_pFrame->SendMessage(WM_COMMAND, ID_PLAYER_STOP);
	
	// and signal mainframe to shutdown the modeless tool.
	// UpateData(TRUE);		/* if have any data to DDX  here */
	if (m_pFrame != NULL)
		m_pFrame->PostMessage(WM_CLOSE_PLAY_DLG, IDCANCEL);
	
	/* CDialog::OnClose(); */
}

// Override to hook this floating desktop window into the frame's command chain.
// means commands fired by buttons in this window can be handled in frame.
// (Ditto for command button update handlers triggered by UpdateDialogControls? 
// But that is unnecessary since frame can be passed 
// as argument to UpdateDialogControls in any case.)
BOOL CPlayDlg::OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo) 
{
	// if we don't handle command, pass along to the designated frame.
	// Doesn't happen automatically since frame is not dialog's parent/owner.	
	return CDialog::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo)
		|| m_pFrame->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo);
}

//
// Command to collect the name of a log file to play and set it into the player control.
//
// Note this is a UI-only change -- it has no effect on the player state! 
// User must hit play to start playing the new file, and might have to
// clean up the main window state for this to work.
// Command is disabled while playback of another file is going on.
//
void CPlayDlg::OnSelectLogfile() 
{
	CFileDialog dlg(TRUE, /* default extension*/ "log", 
                          /* initial filename:*/ "*.log", 
						  /* default flags */ OFN_FILEMUSTEXIST | OFN_HIDEREADONLY | OFN_NOCHANGEDIR, 
						  /* default filters */ NULL,
						  /* parent:*/ this);
	// set initial directory to most recently used, saved in registry
	CString strPlayerDir = theApp.GetProfileString("Settings", "Player Dir", "");
	if (!strPlayerDir.IsEmpty()) 
		dlg.m_ofn.lpstrInitialDir = strPlayerDir;

	// get file name
	if (dlg.DoModal() != IDOK) 
		return;

	// save full path of selected file. This must be be retrieved from the player
	// control by the PLAYER_PLAY command handler (currently in mainframe)
	SetLogFile(dlg.GetPathName());

	// save folder for next open
	strPlayerDir = m_strPathName.Left(m_strPathName.ReverseFind('\\'));
	theApp.WriteProfileString("Settings", "Player Dir", strPlayerDir);

	// for brevity, show filename only in edit box.
	m_strFileName = dlg.GetFileName();
	// etDlgItem(IDC_LOGFILE_NAME)->SetWindowText(dlg.GetFileName());

	// reset goto time
	m_nSecs = 0;
	m_nMins = 0;
	m_nHrs = 0;

	// transfer new member vals into controls:
	UpdateData(FALSE);
}

// Worker routine to update the current log file pathname
void CPlayDlg::SetLogFile(LPCTSTR pszPathName)
{
	// save full path of selected file. This must be be retrieved from the player
	// control by the PLAYER_PLAY command handler (currently in mainframe)
	m_strPathName = pszPathName;

	// for brevity, show filename only in edit box.
	int iLastSlash = m_strPathName.ReverseFind('\\');
	m_strFileName = m_strPathName.Mid(iLastSlash + 1);
	
	// save folder path to start in on next browse from player
	CString strPlayerDir = m_strPathName.Left(iLastSlash);
	theApp.WriteProfileString("Settings", "Player Dir", strPlayerDir);

	// reset goto time for new file
	m_nSecs = 0;
	m_nMins = 0;
	m_nHrs = 0;

	// transfer new member vals into controls:
	UpdateData(FALSE);
}

// Set log file name on drag/drop of file into dialog
void CPlayDlg::OnDropFiles(HDROP hDropInfo) 
{
	// No-op while in playback
	if (! LogPlayerInPlayback()) 
	{
		// take the first filename only
		char szPathName[MAX_PATH] = "";
		::DragQueryFile(hDropInfo, 0, szPathName, MAX_PATH);
		SetLogFile(szPathName);
	}

	// Now done with this information
	::DragFinish(hDropInfo);
}


// Most player controls are forwarded to handlers in the mainframe, which manipulate
// the player state. But we handle the GOTO command here in the dialog since it is not
// parameterless (requires parsing time from control)
void CPlayDlg::OnPlayerGoto() 
{
	UpdateData(TRUE);	// to transfer times and validate.

	LogPlayerGoto(m_nSecs + 60 * m_nMins + 60 * 60 * m_nHrs); 
}


