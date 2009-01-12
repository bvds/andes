// CommentDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "CommentDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCommentDlg dialog
//

// 
// Log playback/recording: We don't want to record edit control contents on every 
// keystroke as done by our logedit, since it could fill pages and pages in the log 
// file. Rather we only want to record a single "SUBMIT-COMMENT" event after OK end with
// the complete comment text (with possible newlines encoded somehow). On playback,
// we want to display the comment text in the dialog the whole time.
// However, we also need to know how long to keep the dialog up.
// So we save start time on initialization and put in a pair of log
// messages on OK corresponding to begin and end of dialog.
 //
// We don't use LogBtns for button controls, since we are not logging presses;
// but we do need button members so type-checking with IsKindOf works on replaying
// button click event inserted in log to close on replay (see LogDialog.cpp).

CCommentDlg::CCommentDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CCommentDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CCommentDlg)
	m_strText = _T("");
	//}}AFX_DATA_INIT

	m_nTimeStart = 0;
}


void CCommentDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CCommentDlg)
	DDX_Control(pDX, IDCANCEL, m_btnCancel);
	DDX_Control(pDX, IDOK, m_btnOK);
	DDX_Text(pDX, IDC_COMMENT_TEXT, m_strText);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CCommentDlg, CLogDialog)
	//{{AFX_MSG_MAP(CCommentDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCommentDlg message handlers

BOOL CCommentDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	
	// Save time dialog opened, for logging at the end
	m_nTimeStart = HistTime();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CCommentDlg::OnOK() 
{
	// Call OnOK first: CLogDialog just updates logPlayer on replay; CDialog
	// does an UpdateData and EndDialog to set end flag.
	CLogDialog::OnOK();

	// Log the beginning of the comment event with comment text 
	// for display on playback, backdating timestamp to start of dialog:
	// NB: Must escape any newlines in m_strText for use in logging message
	LogEventfAt(m_nTimeStart, EV_COMMENT, "%s", EscapeText(m_strText));	
	// Add an entry for the OK button push so it gets closed on playback.
	LogEventf(EV_BTN_CLICK, "%d %s", IDOK , "OK");
}

// Use before initialization on plyaback to set member var from escaped string. 
// Text then transferred into control via normal DDX.
void CCommentDlg::SetText(LPCTSTR pszEscText)
{
	m_strText = UnescapeText(pszEscText);
}

void CCommentDlg::OnCancel() 
{
	// We make log entries on cancel just to show the user worked with the
	// comment dialog for this time. It will replay with a blank field. Though
	// nothing stops us logging the cancelled text! But then wouldn't know user
	// changed mind about this.

	// Log the beginning of the comment event for display on playback.
	LogEventfAt(m_nTimeStart, EV_COMMENT, "");	
	// Add an entry for the Cancel button push so it gets closed on playback.
	LogEventf(EV_BTN_CLICK, "%d %s", IDCANCEL , "Cancel");
	
	CLogDialog::OnCancel();
}
