// HintDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "HintDlg.h"
#include "helpifc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHintDlg dialog

IMPLEMENT_DYNCREATE(CHintDlg, CLogDialog)

CHintDlg::CHintDlg(CWnd* pParent /*=NULL*/)
	: CLogDialog(CHintDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CHintDlg)
	//}}AFX_DATA_INIT
	m_pszHintSpec = NULL;
}


void CHintDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHintDlg)
	DDX_Control(pDX, ID_FOLLOWUP_EXPLAIN, m_btnExplMore);
	DDX_Control(pDX, ID_FOLLOWUP_WHY, m_btnWhy);
	DDX_Control(pDX, ID_FOLLOWUP_HOW, m_btnHow);
	DDX_Control(pDX, IDC_HINT_TEXT, m_editHint);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CHintDlg, CLogDialog)
	//{{AFX_MSG_MAP(CHintDlg)
	ON_BN_CLICKED(ID_FOLLOWUP_EXPLAIN, OnExplainMore)
	ON_BN_CLICKED(ID_FOLLOWUP_HOW, OnBtnHow)
	ON_BN_CLICKED(ID_FOLLOWUP_WHY, OnBtnWhy)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHintDlg message handlers

BOOL CHintDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

	// "Why" button should only be visible if Conceptual help is on.
	if (! (theApp.m_wHelpFlags & fConceptual))
		m_btnWhy.ShowWindow(SW_HIDE);

	// Caller should set an initial HintSpec from help system coming in. 
	// Note we must be prepared to report possible failure return.
	ProcessSpec();
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

// Parse spec and update hint and followup controls
void CHintDlg::ProcessSpec()	
{
	CString strHint;
	CString strButtons;

	if (m_pszHintSpec == NULL )	// Remote call failed 

	{
		strHint = "Sorry, help could not be obtained for that request.";
	}
	else
	{
		// Split quoted string from help system at optional tilde separator into hint
		// and followup button codes 
		CString strSpec = m_pszHintSpec;
		int nSepPos = strSpec.Find('~');
		int nLastPos = strSpec.GetLength() - 1;
		if (nSepPos == -1)						// Separator is optional
			nSepPos = nLastPos + 1;

		strHint = strSpec.Mid(0, nSepPos - 1);	// starts after leading quote
		strButtons = strSpec.Mid(nSepPos + 1);
	}

	// Update hint text in dialog
	m_editHint.SetWindowText(strHint);

	// Enable specified followup buttons
	m_btnHow.EnableWindow(strButtons.Find('h') != -1);
	m_btnExplMore.EnableWindow(strButtons.Find('e') != -1);
	m_btnWhy.EnableWindow(strButtons.Find('w') != -1);
	
}

// For updating current hint. Possible to use before
// Windows window has been created, but see below.
void CHintDlg::SetHintSpec(LPCTSTR pszHintSpec)
{
	m_pszHintSpec = pszHintSpec;

	// Update controls if window is created
	if (m_hWnd != NULL)
		ProcessSpec();
	// else we are saving pointer to someone else's buffer, probably HelpIfc's.
	// Presumably we will be shown soon and the text copied to controls,
	// Should change to copy the string to member. 
}

// Followup commands:
void CHintDlg::OnBtnHow() 
{
	SetHintSpec(HelpSystemExecf("(Hint-Next-Substep)"));
}

void CHintDlg::OnBtnWhy() 
{
	SetHintSpec(HelpSystemExecf("(Why)"));
}

void CHintDlg::OnExplainMore() 
{
	SetHintSpec(HelpSystemExecf("(Explain-More)"));
}





