// TaskDlg.cpp : implementation file
//
// The dialog's function is to select a command for the app to execute. The
// Button Ids should be the command ids to be executed. Clicking any of them
// ends the dialog.
//
#include "stdafx.h"
#include "fbd.h"
#include "history.h"		// required for EventID type before LogDlg defs.
#include "TaskDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTaskDlg dialog


CTaskDlg::CTaskDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CTaskDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CTaskDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CTaskDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTaskDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CTaskDlg, CLogDialog)
	//{{AFX_MSG_MAP(CTaskDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTaskDlg message handlers

BOOL CTaskDlg::OnCommand(WPARAM wParam, LPARAM lParam) 
{
	// Rather than transfer the selected value, we just post a command msg for
	// the mainframe to the App's message queue and end the dialog.
	// !!! could this method cause trouble on log playback?
	WORD wCmdID = LOWORD(wParam);

	// Log it
	CButton* pBtn = (CButton*) GetDlgItem(wCmdID);
	CString strLabel; 
	if (pBtn)
		pBtn->GetWindowText(strLabel); 
	LogEventf(EV_TASK_SELECT, "%d |%s|", wCmdID, strLabel);
	
	// Post the command to the main window
	AfxGetMainWnd()->PostMessage(WM_COMMAND, MAKELONG(wCmdID, 0), (LPARAM) 0);
	// No further dispatching
	CLogDialog::OnOK();
	return TRUE;
}

BOOL CTaskDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	
	LogEventf(EV_DLG_TASK, "");
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
