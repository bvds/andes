// ExpLawDlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "ExpLawDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CExpLawDlg dialog
typedef struct
{	int	id;
	int strId;
} lawType;

lawType Laws[] = {

 ID_EXP_NEWTONSFIRST,				IDS_NEWTONSFIRST,
 ID_EXP_NEWTONSSECOND,				IDS_NEWTONSSECOND,
 ID_EXP_KINEMATICS,					IDS_KINEMATICS,
 ID_EXP_ENERGY,						IDS_ENERGY,
 ID_EXP_NETWORK,					IDS_NETWORK,
};

const int numLaws ARRAY_SIZE(Laws);


CExpLawDlg::CExpLawDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CExpLawDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CExpLawDlg)
	//}}AFX_DATA_INIT
	m_nId = -1;
}


void CExpLawDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExpLawDlg)
	//}}AFX_DATA_MAP
	DDX_Control(pDX, IDC_EXPLAIN_LAW, m_editLaw);

}


BEGIN_MESSAGE_MAP(CExpLawDlg, CDialog)
	//{{AFX_MSG_MAP(CExpLawDlg)
	ON_WM_SHOWWINDOW()
	ON_WM_MOVE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CExpLawDlg message handlers

void CExpLawDlg::PostNcDestroy() 
{
	delete this;	

}


void CExpLawDlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	CDialog::OnShowWindow(bShow, nStatus);
	
	if (bShow)
	{
		UpdateExplanation();	
	}
	
}

void CExpLawDlg::UpdateExplanation()
{
	CString strDef;	
	for (int i=0; i<numLaws; i++){
		if (Laws[i].id == m_nId){
			strDef.LoadString(Laws[i].strId);
			m_editLaw.SetRichEditText(strDef, 100);
			break;
		}
	}
}

void CExpLawDlg::OnMove(int x, int y) 
{
	CDialog::OnMove(x, y);
	
	UpdateExplanation();
	
}
