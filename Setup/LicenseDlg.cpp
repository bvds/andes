// LicenseDlg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "LicenseDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLicenseDlg dialog

#ifndef FAST

CLicenseDlg::CLicenseDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CLicenseDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CLicenseDlg)
	m_strText = _T("");
	m_nYesNo = 1;
	//}}AFX_DATA_INIT
}


void CLicenseDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLicenseDlg)
	DDX_Text(pDX, IDC_LICENSE_TEXT, m_strText);
	DDX_Radio(pDX, IDC_ACCEPT, m_nYesNo);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLicenseDlg, CDialog)
	//{{AFX_MSG_MAP(CLicenseDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLicenseDlg message handlers
#endif
