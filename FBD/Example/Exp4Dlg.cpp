// Exp4Dlg.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "helpifc.h"
#include "EXPlanVw.h"
#include "EXView.h"
#include "DoneDlg.h"
#include "Exp4Dlg.h"
#include "PlanStrs.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CExp4Dlg dialog


CExp4Dlg::CExp4Dlg(CFBDDoc* pDoc, CWnd* pParent /*=NULL*/)
	: CTemplateDlg(CExp4Dlg::IDD, pParent)
{
	m_pParent = pParent;
	m_pDoc=pDoc;
	m_boxes[0] = &m_cboIF;
	m_boxes[1] = &m_cboAND1;
	m_boxes[2] = &m_cboAND2;
	m_boxes[3] = &m_cboDueTo;
	m_statics[0] = &m_stcIF;
	m_statics[1] = &m_stcAND1;
	m_statics[2] = &m_stcAND2;
	m_statics[3] = &m_stcThen;
	//{{AFX_DATA_INIT(CExp4Dlg)
	//}}AFX_DATA_INIT
}


void CExp4Dlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CExp4Dlg)
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_THEN, m_stcThen);
	DDX_Control(pDX, IDC_SUBMIT, m_Submit);
	DDX_Control(pDX, IDC_IF, m_stcIF);
	DDX_Control(pDX, IDC_COMBO4, m_cboDueTo);
	DDX_Control(pDX, IDC_COMBO3, m_cboAND2);
	DDX_Control(pDX, IDC_COMBO2, m_cboAND1);
	DDX_Control(pDX, IDC_COMBO1, m_cboIF);
	DDX_Control(pDX, IDC_BACK, m_Back);
	DDX_Control(pDX, IDC_AND2, m_stcAND2);
	DDX_Control(pDX, IDC_AND1, m_stcAND1);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CExp4Dlg, CDialog)
	//{{AFX_MSG_MAP(CExp4Dlg)
	ON_WM_SHOWWINDOW()
	ON_BN_CLICKED(IDC_BACK, OnBack)
	ON_BN_CLICKED(IDC_SUBMIT, OnSubmit)
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
	ON_CONTROL_RANGE(CBN_SELCHANGE, IDC_COMBO1, IDC_COMBO4, OnSelchangeCombo)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CExp4Dlg message handlers

void CExp4Dlg::OnShowWindow(BOOL bShow, UINT nStatus) 
{
	if (bShow)
	{
		InitTemplate();
		InsertCtrlStrs();
		UpdateCombos(4);
	}
	CLogDialog::OnShowWindow(bShow, nStatus);
	
}

void CExp4Dlg::InsertCtrlStrs()
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
			ASSERT(count < 5);
			if (count == 4){
				m_stcThen.SetWindowText(prepStr);
				count = 5;
			}
			else{
				pStc = m_statics[count];
				pBox = m_boxes[count];
				count = count + 1;					
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

void CExp4Dlg::OnSubmit() 
{
	if (!IsRightRule())//(base class function)
		return;
	
	if (IsAllCorrect(4)){//check all 4 combo boxes (base class function)
		UpdateBtns(FALSE);//update submit, cancel, close (bReset = FALSE)
		UpdateEXInfo(4);//update with infor from 4 combos (base class function)
	}
		
	Invalidate();
	
}

void CExp4Dlg::PostNcDestroy() 
{
	delete this;
}

void CExp4Dlg::ResetTemplate()
{
	for (int i = 0; i <4; i++){
		m_boxes[i]->ResetContent();
	}
	for (int j = 0; j < 4; j++){
		m_statics[j]->SetWindowText("");
	}

	UpdateBtns(TRUE);//we are resetting the dialog (bReset = TRUE)
}
