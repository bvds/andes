// SolveVarDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "SolveVarDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSolveVarDlg dialog


CSolveVarDlg::CSolveVarDlg(CStringList* pStrList, CWnd* pParent /*=NULL*/)
	: CLogDialog(CSolveVarDlg::IDD, pParent)
{
	m_pStrList = pStrList;
	//{{AFX_DATA_INIT(CSolveVarDlg)
	//}}AFX_DATA_INIT
}


void CSolveVarDlg::DoDataExchange(CDataExchange* pDX)
{
	CLogDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CSolveVarDlg)
	DDX_Control(pDX, IDC_VARIABLES, m_cboVarList);
	//}}AFX_DATA_MAP
	DDX_FillList(pDX, IDC_VARIABLES, m_pStrList);
	DDX_CBStringExact(pDX, IDC_VARIABLES, m_strVar);
}


BEGIN_MESSAGE_MAP(CSolveVarDlg, CDialog)
	//{{AFX_MSG_MAP(CSolveVarDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSolveVarDlg message handlers

BOOL CSolveVarDlg::OnInitDialog() 
{
	CLogDialog::OnInitDialog();
	
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
