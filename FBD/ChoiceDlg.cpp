// ChoiceDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ChoiceDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CChoiceDlg dialog


CChoiceDlg::CChoiceDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CChoiceDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CChoiceDlg)
	m_bCorrect = FALSE;
	m_strText = _T("");
	m_bChosen = FALSE;
	//}}AFX_DATA_INIT
}


void CChoiceDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CChoiceDlg)
	DDX_Check(pDX, IDC_CORRECT_CHOICE, m_bCorrect);
	DDX_Text(pDX, IDC_EDIT1, m_strText);
	DDX_Check(pDX, IDC_CHOSEN, m_bChosen);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CChoiceDlg, CDialog)
	//{{AFX_MSG_MAP(CChoiceDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CChoiceDlg message handlers
