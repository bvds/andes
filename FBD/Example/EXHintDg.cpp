// EXHintDg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "EXHintDg.h"
#include "helpifc.h"



#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEXHintDlg dialog


CEXHintDlg::CEXHintDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CEXHintDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CEXHintDlg)
	//}}AFX_DATA_INIT
}


void CEXHintDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CEXHintDlg)
	DDX_Control(pDX, ID_YES, m_btnYes);
	DDX_Control(pDX, IDOK, m_btnOK);
	DDX_Control(pDX, IDC_HINT_TEXT, m_editHint);
	DDX_Control(pDX, IDC_NOTHANKS, m_btnNoThanks);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CEXHintDlg, CLogDialog)
	//{{AFX_MSG_MAP(CEXHintDlg)
	ON_BN_CLICKED(IDC_NOTHANKS, OnNothanks)
	ON_BN_CLICKED(ID_YES, OnYes)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEXHintDlg message handlers


void CEXHintDlg::OnClarify() 
{
	CString strHint;
	CString strButtons;
	int nAsteriskPos;
  	if (m_pszHintSpec == NULL)	// Remote call failed or
	{
	     strHint = "Sorry, help could not be obtained for that request.";
	}
    else
	{
	 // Split quoted string from help system at optional tilde separator into hint
	  // strID and menuID; 
		CString strSpec = m_pszHintSpec;
		nAsteriskPos = strSpec.Find('*');
		int nLastPos = strSpec.GetLength() - 1;
		if (nAsteriskPos != -1){
			strButtons = strSpec.Right(nLastPos-nAsteriskPos);
			strSpec = strSpec.Left(nAsteriskPos);
		}
		int nSepPos = strSpec.Find('~');
		if (nSepPos == -1)						// Separator is optional
			nSepPos = nAsteriskPos - 1;
		ASSERT(strSpec[0] != '|');
		ASSERT(strSpec[0] != '\"');
		strHint = strSpec.Mid(0, nSepPos);//instructions shown in hintdlg	
		m_strHints = strSpec.Mid(nSepPos + 1);//list of id passed to HighlighHint
			
	}
//	strHint = "\"" + strHint;
	strcpy((LPTSTR)m_pszHintSpec, strHint);	
	// Update hint text in dialog
	m_editHint.SetWindowText(strHint);
	// Enable specified followup buttons
	if (nAsteriskPos != -1){
		m_btnYes.EnableWindow(strButtons.Find('y') != -1);
		m_btnOK.EnableWindow(strButtons.Find('o') != -1);
		m_btnNoThanks.EnableWindow(strButtons.Find('n') != -1);
	}
	m_editHint.UpdateWindow();
	
}


void CEXHintDlg::OnNothanks() 
{
//	m_pszHintSpec = HelpSystemExecf("(no-thanks)");
	Logf("no-thanks");
//	EndDialog(ID_NOTHANKS);
	CLogDialog::OnOK();
	
}

void CEXHintDlg::OnOK() 
{
//	m_pszHintSpec = HelpSystemExecf("(ok)");
	CLogDialog::OnOK();
}

BOOL CEXHintDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	Logf("EX-Hint-Dlg");
	
	// Caller should set an initial HintSpec from help system coming in. 
	// Note we must be prepared to handle possible failure return.
	OnClarify();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


void CEXHintDlg::OnYes() 
{	
	Logf("Yes");
	EndDialog(ID_NOTHANKS);
}
