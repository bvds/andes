// SettingPg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "InstDirPg.h"
#include "SettingPg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSettingPg property page

IMPLEMENT_DYNCREATE(CSettingPg, CPropertyPage)

CSettingPg::CSettingPg() : CPropertyPage(CSettingPg::IDD)
{
	//{{AFX_DATA_INIT(CSettingPg)
	//}}AFX_DATA_INIT
}

CSettingPg::~CSettingPg()
{
}

void CSettingPg::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSettingPg)
	DDX_Control(pDX, IDC_CUR_SETTINGS, m_ctrlSettings);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CSettingPg, CPropertyPage)
	//{{AFX_MSG_MAP(CSettingPg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSettingPg message handlers

BOOL CSettingPg::OnSetActive() 
{
	CWnd* pWnd = GetParent();
	CPropertySheet* pSht = (CPropertySheet*)pWnd;
	pSht->SetWizardButtons(PSWIZB_NEXT|PSWIZB_BACK);
	//page 3, both buttons active,  get selected info from previous 
	//page
	CString str;
	str.Format("Setup Type\n  Complete\n\nTarget Directory\n   %s", m_strInstDir);
	m_ctrlSettings.SetWindowText(str);	
	return CPropertyPage::OnSetActive();
}


LRESULT CSettingPg::OnWizardNext() 
{
#ifndef FAST  // do actions in InstDirPg
	CWnd* pWnd = GetParent();
	CPropertySheet* pSht = (CPropertySheet*)pWnd;
	//start copying the files
	if (!theApp.CopyTheFiles())
		pSht->EndDialog(IDCANCEL);
#endif
	return CPropertyPage::OnWizardNext();
}

BOOL CSettingPg::OnInitDialog() 
{
	CPropertyPage::OnInitDialog();

#ifndef FAST // do this on InstDirPg
	m_strInstDir = theApp.m_strInstDir;
#endif

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
