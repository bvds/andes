// Cleanup.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "Cleanup.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCleanup dialog


CCleanup::CCleanup(CWnd* pParent /*=NULL*/)
	: CDialog(CCleanup::IDD, pParent)
{
	//{{AFX_DATA_INIT(CCleanup)
	m_strText = _T("");
	m_bRestart = FALSE;
	//}}AFX_DATA_INIT
}


void CCleanup::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CCleanup)
	DDX_Control(pDX, IDC_STATIC_TXT, m_stcText);
	DDX_Text(pDX, IDC_STATIC_TXT, m_strText);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CCleanup, CDialog)
	//{{AFX_MSG_MAP(CCleanup)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCleanup message handlers

void CCleanup::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CDialog::OnShowWindow(bShow, nStatus);
	
	if (bShow && m_bRestart)
		SetWindowText("Setup Restart");
	
}
