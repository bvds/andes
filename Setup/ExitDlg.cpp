// ExitDlg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "ExitDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CExitDlg dialog


CExitDlg::CExitDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CExitDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CExitDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CExitDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExitDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CExitDlg, CDialog)
	//{{AFX_MSG_MAP(CExitDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CExitDlg message handlers
