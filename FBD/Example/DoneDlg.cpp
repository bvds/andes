// DoneDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "PlanView.h"
#include "EXView.h"
#include "DoneDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDoneDlg dialog

CDoneDlg::CDoneDlg(CFBDDoc* pDoc , CWnd* pParent /*=NULL*/)
	: CLogDialog(CDoneDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CDoneDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CDoneDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CDoneDlg)
	DDX_Control(pDX, IDOK, m_Ok);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CDoneDlg, CDialog)
	//{{AFX_MSG_MAP(CDoneDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDoneDlg message handlers

void CDoneDlg::OnOK() 
{
	CPlanView* pView = (CPlanView*)theApp.GetPlanView();
//	pView->OnDone();	
//	CDialog::OnOK();
}

void CDoneDlg::PostNcDestroy() 
{
	delete this;
	
//	CDialog::PostNcDestroy();
}


BOOL CDoneDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();


	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

/////////////////////////////////////////////////////////////////////////////
// CExplainDlg dialog


CExplainDlg::CExplainDlg(CFBDDoc* pDoc , CWnd* pParent /*=NULL*/)
	: CLogDialog(CExplainDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CExplainDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CExplainDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExplainDlg)
	DDX_Control(pDX, IDOK, m_Ok);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CExplainDlg, CDialog)
	//{{AFX_MSG_MAP(CExplainDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CExplainDlg message handlers

void CExplainDlg::OnOK() 
{
	CEXView* pView = (CEXView*)theApp.GetEXView();
	pView->OnExplainBtn();
	
	CDialog::OnOK();
}

void CExplainDlg::PostNcDestroy() 
{
	// TODO: Add your specialized code here and/or call the base class
	delete this;
//	CDialog::PostNcDestroy();
}
