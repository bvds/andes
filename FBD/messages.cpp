// messages.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "messages.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInstructMsg dialog


CInstructMsg::CInstructMsg(CFBDDoc* pDoc , CWnd* pParent /*=NULL*/)
	: CLogDialog(CInstructMsg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CInstructMsg)
	m_message = _T("");
	//}}AFX_DATA_INIT
}


void CInstructMsg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CInstructMsg)
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Text(pDX, IDC_MESSAGE, m_message);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CInstructMsg, CLogDialog)
	//{{AFX_MSG_MAP(CInstructMsg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CInstructMsg message handlers
/////////////////////////////////////////////////////////////////////////////
BOOL CInstructMsg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	// only a trace msg on dialog init. DoModal replayed when cause is replayed.
	LogEventf(EV_DLG_INSTRUCT, EscapeText(m_message));	
	MessageBeep(MB_OK);		// generates default sound 

	// TODO: Add extra initialization here
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


/////////////////////////////////////////////////////////////////////////
// CWarningMsg dialog
//
////////////////////////////////////////////////////////////////////////

CWarningMsg::CWarningMsg(CFBDDoc* pDoc , CWnd* pParent /*=NULL*/)
	: CLogDialog(CWarningMsg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CWarningMsg)
	m_message = _T("");
	m_nType = MB_OK;
	//}}AFX_DATA_INIT
}


void CWarningMsg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CWarningMsg)
	DDX_Control(pDX, IDOK, m_Ok);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_MESSAGE, m_stcMessage);
	DDX_Control(pDX, IDC_PICTURE, m_picture);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CWarningMsg, CLogDialog)
	//{{AFX_MSG_MAP(CWarningMsg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CWarningMsg message handlers

BOOL CWarningMsg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	// Only trace msg on dialog init. DoModal replayed when cause is replayed
	// Include message for human reader, escaping in case it is multiline.
	LogEventf(EV_DLG_WARNING, EscapeText(m_message));	
	
	CString strCaption = AfxGetAppName();

	CWnd* pCtrl = GetDlgItem(IDC_MESSAGE );
	// grab styles of existing control
	DWORD dwStyles = ::GetWindowLong(pCtrl->m_hWnd, GWL_STYLE) | ES_MULTILINE;
	// get the existing control's position on the form
	CRect rcEdit;
	pCtrl->GetWindowRect(rcEdit);
	ScreenToClient(rcEdit);
	// get rid of existing control
	pCtrl->DestroyWindow();
	// Create new rich edit w/same style, position, & id
	m_stcMessage.Create(dwStyles, rcEdit, this, IDC_ANGLE_NAME);
	// set to read only 
	m_stcMessage.SetReadOnly();
	// set it to use the dialog's font
	m_stcMessage.SetFont(GetFont());
// AW: leave it enabled to avoid hard-to-read gray text. 
//	m_stcMessage.EnableWindow(FALSE);// (no caret)
	m_stcMessage.SetRichEditText(m_message);
	// make same color as dlg
	m_stcMessage.SetBackgroundColor(FALSE, ::GetSysColor(COLOR_BTNFACE));

	if (m_nType == MB_OK){
		m_Cancel.ShowWindow(FALSE);
		CenterButton(&m_Ok);
	}
	else if (m_nType == MB_YESNO)
	{
		m_Ok.SetWindowText("Yes");
		m_Cancel.SetWindowText("No");
	}


	
	if (m_bInfo)	// informational message only
	{
		strCaption += " Note";
		SetWindowText(strCaption);

		HICON hIcon = theApp.LoadStandardIcon(IDI_ASTERISK);
		m_picture.SetIcon(hIcon);

		MessageBeep(MB_ICONASTERISK);
	}
	else			// warning message
	{
		SetWindowText(strCaption);
		HICON hIcon = theApp.LoadStandardIcon(IDI_EXCLAMATION);

		MessageBeep(MB_ICONEXCLAMATION);
	}
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


void CWarningMsg::CenterButton(CButton* pBtn)
{
	CRect dlgRect, btnRect;

	GetWindowRect(&dlgRect);
	pBtn->GetWindowRect(&btnRect);
	int midDlg = (dlgRect.right + dlgRect.left)/2;
	int heightBtn=btnRect.bottom-btnRect.top;
	int widthBtn=btnRect.right-btnRect.left;
	int halfBtn = (widthBtn)/2;
	int leftPos =midDlg-halfBtn-dlgRect.left;
	int topPos=btnRect.top-dlgRect.top-heightBtn;
	pBtn->MoveWindow(leftPos, topPos, widthBtn, heightBtn,TRUE);
}
