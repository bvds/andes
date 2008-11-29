// FinishPg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "Cleanup.h"
#include "FinishPg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CFinishPg property page

IMPLEMENT_DYNCREATE(CFinishPg, CPropertyPage)

CFinishPg::CFinishPg() : CPropertyPage(CFinishPg::IDD)
{
	//{{AFX_DATA_INIT(CFinishPg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

CFinishPg::~CFinishPg()
{
}

void CFinishPg::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CFinishPg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CFinishPg, CPropertyPage)
	//{{AFX_MSG_MAP(CFinishPg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CFinishPg message handlers

BOOL CFinishPg::OnSetActive() 
{
	CWnd* pWnd = GetParent();
	CPropertySheet* pSht = (CPropertySheet*)pWnd;
	//last page, finish button only button active
	pSht->SetWizardButtons(PSWIZB_FINISH);
	CancelToClose();
	return CPropertyPage::OnSetActive();
}
