// AreaDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "AreaDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CAreaDlg dialog


CAreaDlg::CAreaDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CAreaDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CAreaDlg)
	m_strName = _T("");
	m_bBorder = FALSE;
	//}}AFX_DATA_INIT
}


void CAreaDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAreaDlg)
	DDX_CBStringExact(pDX, IDC_AREA_CHOICE, m_strName);
	DDX_Check(pDX, IDC_BORDER, m_bBorder);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CAreaDlg, CDialog)
	//{{AFX_MSG_MAP(CAreaDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CAreaDlg message handlers
