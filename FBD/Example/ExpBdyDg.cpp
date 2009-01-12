// ExpBdyDg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "helpifc.h"
#include "EXPlanVw.h"
#include "EXView.h"
#include "DoneDlg.h"
#include "ExpBdyDg.h"
#include "PlanStrs.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CExplainBodyDlg dialog


CExplainBodyDlg::CExplainBodyDlg(CFBDDoc* pDoc, CWnd* pParent /*=NULL*/)
	: CTemplateDlg(CExplainBodyDlg::IDD, pParent)
{
	m_pParent = pParent;
	m_pDoc=pDoc;
	m_boxes[0] = &m_cboIF;
	m_boxes[1] = &m_cboAND1;
	m_boxes[2] = &m_cboDueTo;
	m_boxes[3] = &m_cboExertedBy;
	m_statics[0] = &m_stcIF;
	m_statics[1] = &m_stcAND1;
	m_statics[2] = &m_stcDueTo;
	m_statics[3] = &m_stcExertedBy;
	m_statics[4] = &m_stcThen;
	//{{AFX_DATA_INIT(CExplainBodyDlg)
	//}}AFX_DATA_INIT
}


void CExplainBodyDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExplainBodyDlg)
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_BACK, m_Back);
	DDX_Control(pDX, IDC_SUBMIT, m_Submit);
	DDX_Control(pDX, IDC_THEN, m_stcThen);
	DDX_Control(pDX, IDC_IF, m_stcIF);
	DDX_Control(pDX, IDC_EXERTED_BY, m_stcExertedBy);
	DDX_Control(pDX, IDC_DUETO, m_stcDueTo);
	DDX_Control(pDX, IDC_AND1, m_stcAND1);
	DDX_Control(pDX, IDC_COMBO4, m_cboExertedBy);
	DDX_Control(pDX, IDC_COMBO3, m_cboDueTo);
	DDX_Control(pDX, IDC_COMBO2, m_cboAND1);
	DDX_Control(pDX, IDC_COMBO1, m_cboIF);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CExplainBodyDlg, CLogDialog)
	//{{AFX_MSG_MAP(CExplainBodyDlg)
	ON_WM_CTLCOLOR()
	ON_BN_CLICKED(IDC_SUBMIT, OnSubmit)
	ON_WM_SHOWWINDOW()
	ON_BN_CLICKED(IDC_BACK, OnBack)
	//}}AFX_MSG_MAP
	ON_CONTROL_RANGE(CBN_SELCHANGE, IDC_COMBO1, IDC_COMBO4, OnSelchangeCombo)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CExplainBodyDlg message handlers


void CExplainBodyDlg::OnSubmit() 
{
	if (!IsRightRule())
		return;
	
	if (IsAllCorrect(4)){
		UpdateBtns(FALSE);//update submit, cancel, close (bReset = FALSE)
		UpdateEXInfo(4);
	}
		
	Invalidate();

}

void CExplainBodyDlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	if (bShow)
	{
		InitTemplate();
		InsertCtrlStrs();
		UpdateCombos(4);
	}
	CLogDialog::OnShowWindow(bShow, nStatus);

}

void CExplainBodyDlg::InsertCtrlStrs()
{
	CStatic* pStc;
	CLogCombo* pBox;

	int count = 0;
	for (int i=0; i<NRULESETUPS; i++)
	{
		CString prepStr = ruleSetup[i].prepStr;
		int nrule = ruleSetup[i].nRule;
		if (m_rule == ID_UNDEF)
			m_stcThen.SetWindowText("TEMPLATE UNDER CONSTRUCTION");
			else if (nrule == m_rule){
			if (count == 2){
				m_stcThen.SetWindowText(prepStr);
				count = 3;
			}
			else{
				if (count == 0){
					pStc = m_statics[0];
					pBox = m_boxes[0];
					count = 1;					
				}
				else if (count == 1){
					pStc = m_statics[1];
					pBox = m_boxes[1];
					count = 2;
					}
				else if (count == 3){
					pStc = m_statics[2];
					pBox = m_boxes[2];
					count =4;
				}
				else{
					pStc = m_statics[3];
					pBox = m_boxes[3];
				}
				pStc->SetWindowText(prepStr);
				for (int j=0; j<NTMPLSTRS; j++){
					CString strCat = tmplStrs[j].catStrs;
					int id = tmplStrs[j].catID;
					if (id == ruleSetup[i].catID){
						int k=0;
						pBox->InsertString(k, strCat);
						k++;
					}
				}
			}
		}
	}

}

void CExplainBodyDlg::PostNcDestroy() 
{
	// TODO: Add your specialized code here and/or call the base class
	delete this;
//	CLogDialog::PostNcDestroy();
}

void CExplainBodyDlg::ResetTemplate()
{
	for (int i = 0; i <4; i++){
		m_boxes[i]->ResetContent();
	}
	for (int j = 0; j < 5; j++){
		m_statics[j]->SetWindowText("");
	}
	UpdateBtns(TRUE);//we are resetting the dialog (bReset = TRUE)
}
