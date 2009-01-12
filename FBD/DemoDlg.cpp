// DemoDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "Mainfrm.h"
#include "DemoDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDemoDlg dialog


// custom constructor for modeless dlg, sets frame to notify on close
// That need not be parent!
CDemoDlg::CDemoDlg(CFrameWnd* pFrame) 
	: CDialog(CDemoDlg::IDD)	// 
{
	m_pFrame = pFrame; 
	//{{AFX_DATA_INIT(CDemoDlg)
	//}}AFX_DATA_INIT
}

void CDemoDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDemoDlg)
	DDX_Control(pDX, IDC_LOGTIME_TEXT, m_txtTime);
	DDX_Control(pDX, IDC_DEMO_SLIDER, m_slider);
	//}}AFX_DATA_MAP
}

BOOL CDemoDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	// set range to show percentage 
	m_slider.SetRange(0, 100, FALSE);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

BEGIN_MESSAGE_MAP(CDemoDlg, CDialog)
	//{{AFX_MSG_MAP(CDemoDlg)
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDemoDlg message handlers

void CDemoDlg::OnClose() // user hit close button
{
	// treat as just another way to generate a QUIT_DEMO command.
	// Frame's handler should destroy us when exiting demo mode.
	// note we never destroy ourselves anymore, so don't send CLOSE msgs to frame.
	m_pFrame->SendMessage(WM_COMMAND, ID_PLAYER_QUIT_DEMO);
}

BOOL CDemoDlg::OnCmdMsg(UINT nID, int nCode, void* pExtra, AFX_CMDHANDLERINFO* pHandlerInfo) 
{
	// if dialog doesn't handle command, route it to our designated frame.
	// Doesn't happen automatically since it may not be dialog's parent/owner.
	return CDialog::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo)
		|| (m_pFrame && m_pFrame->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo));
}

void CDemoDlg::UpdatePlayerUI()
{
	UpdateDialogControls(m_pFrame, TRUE);
}

void CDemoDlg::SetPlaybackTime(LPCTSTR szTime)
{
	if (GetSafeHwnd() != 0)			// Dialog window is attached
		m_txtTime.SetWindowText(szTime);
}

void CDemoDlg::SetProgress(int nPercent)
{
	if (nPercent < 0) 
		nPercent = 0;
	else if (nPercent > 100)
		nPercent = 100;

	m_slider.SetPos(nPercent);
}

// demo completed normally.
// 
void CDemoDlg::NotifyFinished()	
{
	AfxMessageBox("Demo Over");
	// tell frame to exit demo mode, same as Close and QUIT_DEMO commands.
	OnClose();
}


