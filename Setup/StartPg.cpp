// StartPg.cpp : implementation file
//

#include "stdafx.h"
#include <winver.h>
#include "setup.h"
#include "ExitDlg.h"
#include "StartPg.h"
#include "LicenseDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CStartPg property page

IMPLEMENT_DYNCREATE(CStartPg, CPropertyPage)

CStartPg::CStartPg() : CPropertyPage(CStartPg::IDD)
{
	//{{AFX_DATA_INIT(CStartPg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

CStartPg::~CStartPg()
{
}

void CStartPg::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStartPg)
	DDX_Control(pDX, IDC_VERSION, m_stcVersion);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStartPg, CPropertyPage)
	//{{AFX_MSG_MAP(CStartPg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStartPg message handlers

BOOL CStartPg::OnSetActive() 
{
	CWnd* pWnd = GetParent();
	CPropertySheet* pSht = (CPropertySheet*)pWnd;
	//First page, back button diasabled. Next button active.
	pSht->SetWizardButtons(PSWIZB_NEXT);
	return CPropertyPage::OnSetActive();
}





BOOL CStartPg::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();
	CString strFBDVersion = theApp.GetProductVersion("FBD-tcp.exe");	
	m_stcVersion.SetWindowText(strFBDVersion);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


LRESULT CStartPg::OnWizardNext() 
{
	// TODO: Add your specialized code here and/or call the base class

	// Run the license modal dialog before going on to the next screen.
	if (! theApp.AcceptLicense())
		return -1;

	return CPropertyPage::OnWizardNext();
}
