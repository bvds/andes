// ChkSpaceDlg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "MySheet.h"
#include "ChkSpaceDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CChkSpaceDlg dialog


CChkSpaceDlg::CChkSpaceDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CChkSpaceDlg::IDD, pParent)
{
	m_pSht = (CMySheet*)pParent;
	//{{AFX_DATA_INIT(CChkSpaceDlg)
	//}}AFX_DATA_INIT
}


void CChkSpaceDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CChkSpaceDlg)
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CChkSpaceDlg, CDialog)
	//{{AFX_MSG_MAP(CChkSpaceDlg)
	ON_WM_SHOWWINDOW()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChkSpaceDlg message handlers

void CChkSpaceDlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CDialog::OnShowWindow(bShow, nStatus);
	
	if (bShow)
		m_pSht->ShowWindow(SW_HIDE);
	else
		m_pSht->ShowWindow(SW_SHOW);
	
}

void CChkSpaceDlg::PostNcDestroy() 
{
	delete this;	

	CDialog::PostNcDestroy();
}
