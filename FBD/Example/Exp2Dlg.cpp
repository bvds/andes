// Exp2Dlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "helpifc.h"
#include "EXPlanVw.h"
#include "EXView.h"
#include "DoneDlg.h"
#include "Exp2Dlg.h"
#include "PlanStrs.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CExp2Dlg dialog


CExp2Dlg::CExp2Dlg(CFBDDoc* pDoc, CWnd* pParent /*=NULL*/)
	: CTemplateDlg(CExp2Dlg::IDD, pParent)
{
	m_pParent = pParent;
	m_pDoc=pDoc;
	m_boxes[0] = &m_cboIF;
	m_boxes[1] = &m_cboAND1;
	m_statics[0] = &m_stcIF;
	m_statics[1] = &m_stcAND1;
	m_statics[2] = &m_stcThen;
	//{{AFX_DATA_INIT(CExp2Dlg)
	//}}AFX_DATA_INIT
}


void CExp2Dlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExp2Dlg)
	DDX_Control(pDX, IDC_BACK, m_Back);
	DDX_Control(pDX, IDC_AND1, m_stcAND1);
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_THEN, m_stcThen);
	DDX_Control(pDX, IDC_SUBMIT, m_Submit);
	DDX_Control(pDX, IDC_IF, m_stcIF);
	DDX_Control(pDX, IDC_COMBO2, m_cboAND1);
	DDX_Control(pDX, IDC_COMBO1, m_cboIF);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CExp2Dlg, CDialog)
	//{{AFX_MSG_MAP(CExp2Dlg)
	ON_BN_CLICKED(IDC_BACK, OnBack)
	ON_BN_CLICKED(IDC_SUBMIT, OnSubmit)
	ON_WM_SHOWWINDOW()
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
	ON_CONTROL_RANGE(CBN_SELCHANGE, IDC_COMBO1, IDC_COMBO2, OnSelchangeCombo)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CExp2Dlg message handlers
void CExp2Dlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	if (bShow)
	{
		InitTemplate();
		InsertCtrlStrs();
		UpdateCombos(2);
	}
	CLogDialog::OnShowWindow(bShow, nStatus);

}

void CExp2Dlg::InsertCtrlStrs()
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
			if (count == 1){
				m_stcThen.SetWindowText(prepStr);
				count = 2;
			}
			else{
				if (count == 0){
					pStc = m_statics[0];
					pBox = m_boxes[0];
					count = 1;					
				}
				else if (count == 2){
					pStc = m_statics[1];
					pBox = m_boxes[1];
					count = 3;
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


void CExp2Dlg::OnSubmit() 
{
	if (!IsRightRule())
		return;
	
	if (IsAllCorrect(2)){// check both combos
		UpdateBtns(FALSE);//update submit cancel close (bReset = FALSE)
		UpdateEXInfo(2);//update EXInfo with info from both combos
	}

	Invalidate();
	
}


void CExp2Dlg::PostNcDestroy() 
{
	delete this;
}

void CExp2Dlg::ResetTemplate()
{
	for (int i = 0; i <2; i++){
		m_boxes[i]->ResetContent();
	}
	for (int j = 0; j < 3; j++){
		m_statics[j]->SetWindowText("");
	}
	UpdateBtns(TRUE);//we are resetting the dialog (bReset = TRUE)
}
