// VideoDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "VideoDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVideoDlg dialog


CVideoDlg::CVideoDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CVideoDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CVideoDlg)
	m_nChoice = -1;
	//}}AFX_DATA_INIT
}


void CVideoDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVideoDlg)
	DDX_Radio(pDX, IDC_RADIO1, m_nChoice);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CVideoDlg, CDialog)
	//{{AFX_MSG_MAP(CVideoDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVideoDlg message handlers
