// InstDirPg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "SettingPg.h"
#include "InstDirPg.h"
#include "DirectoryDlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInstDirPg property page

IMPLEMENT_DYNCREATE(CInstDirPg, CPropertyPage)

CInstDirPg::CInstDirPg() : CPropertyPage(CInstDirPg::IDD)
{
	//{{AFX_DATA_INIT(CInstDirPg)
	m_strInstDir = _T("");
	//}}AFX_DATA_INIT
}

CInstDirPg::~CInstDirPg()
{
}

void CInstDirPg::DoDataExchange(CDataExchange* pDX)
{
	CPropertyPage::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CInstDirPg)
	DDX_Control(pDX, IDC_INSTALLDIR, m_ctrlInstDir);
	DDX_Text(pDX, IDC_INSTALLDIR, m_strInstDir);
	DDV_MaxChars(pDX, m_strInstDir, 255);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CInstDirPg, CPropertyPage)
	//{{AFX_MSG_MAP(CInstDirPg)
	ON_BN_CLICKED(IDC_BROWSE, OnBrowse)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CInstDirPg message handlers

LRESULT CInstDirPg::OnWizardNext() 
{
	CString strInstDir;
	m_ctrlInstDir.GetWindowText(strInstDir);
	theApp.m_strInstDir = strInstDir;
#if 0 // this is not reliable
	//Check for sufficient disk space
	if (!theApp.CheckDiskSpace()){
		return -1;
	}
#endif 0 
	return CPropertyPage::OnWizardNext();
}

void CInstDirPg::OnBrowse() 
{
	//bring up the directory dialog,
	//The user can browse their system or the network for an 
	//installation directory
	CDirectoryDlg dlg(this);
	dlg.m_strPath = m_strInstDir;
	dlg.DoModal();
	m_ctrlInstDir.SetWindowText(m_strInstDir);
}

BOOL CInstDirPg::OnSetActive() 
{
	CWnd* pWnd = GetParent();
	CPropertySheet* pSht = (CPropertySheet*)pWnd;
	//2nd page, both back and next button enabled
	pSht->SetWizardButtons(PSWIZB_NEXT|PSWIZB_BACK);
	//Set default install directory
	if (m_strInstDir.IsEmpty()) {
#ifdef OLI
		m_strInstDir = "C:\\AndesOLI";
#else 
		m_strInstDir = "C:\\AndesDemo";        // "C:\\Program Files\\Andes"; 
#endif

    }
	m_ctrlInstDir.SetWindowText(m_strInstDir);

	return CPropertyPage::OnSetActive();
}

