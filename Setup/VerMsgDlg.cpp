// VerMsgDlg.cpp : implementation file
//

#include "stdafx.h"
#include "setup.h"
#include "VerMsgDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CVerMsgDlg dialog


CVerMsgDlg::CVerMsgDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CVerMsgDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CVerMsgDlg)
	m_strMsg = _T("");
	//}}AFX_DATA_INIT
}


void CVerMsgDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CVerMsgDlg)
	DDX_Control(pDX, IDC_STATIC_MSG, m_stcMsg);
	DDX_Text(pDX, IDC_STATIC_MSG, m_strMsg);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CVerMsgDlg, CDialog)
	//{{AFX_MSG_MAP(CVerMsgDlg)
	ON_BN_CLICKED(IDC_YESTOALL, OnYestoall)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CVerMsgDlg message handlers

void CVerMsgDlg::OnYestoall() 
{
	EndDialog(ID_SKIPALL);
}
