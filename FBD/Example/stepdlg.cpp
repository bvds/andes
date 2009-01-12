// Step1Dlg.cpp : implementation file
//
//$Id: stepdlg.cpp,v 1.1 2005/01/24 16:28:10 bvds Exp $
#include "stdafx.h"
#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"
#include "StepDlg.h"
#include "PlanStrs.h"
#include "PlanView.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CStep1Dlg dialog


CStepDlg::CStepDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CStepDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CStepDlg)
	m_strStep = "";
	//}}AFX_DATA_INIT
}


void CStepDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStep1Dlg)
	DDX_Control(pDX, IDCANCEL, m_Cancel);
	DDX_Control(pDX, IDC_LIST1, m_listStep);
	DDX_LBStringExact(pDX, IDC_LIST1, m_strStep);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStepDlg, CDialog)
	//{{AFX_MSG_MAP(CStepDlg)
	ON_LBN_DBLCLK(IDC_LIST1, OnDblclkList1)
	ON_BN_CLICKED(IDC_BACK, OnBack)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStepDlg message handlers

BOOL CStepDlg::OnInitDialog() 
{
		
	CDialog::OnInitDialog();
	//fill in list box
	if (m_nLevel == LEVEL_PRINCIPLE){
		for (int i=0; i<NSTEPSTRINGS; i++){
			m_strStep = stepStrings[i].listItem;
			m_listStep.InsertString(i, m_strStep);
			m_listStep.SetItemData(i, stepStrings[i].stepID);
		}
	}
	else if (m_nLevel == LEVEL_STEP){
		for (int i=0; i<NMIDSTEPSTRINGS; i++)
		{
			m_strStep = midstepStrings[i].listItem;
			m_listStep.InsertString(i, m_strStep);
			m_listStep.SetItemData(i, midstepStrings[i].stepID);
		}
	}
	else if (m_nLevel == LEVEL_SUBSTEP)
	{
		int k;
		for (int i=0; i<NSUBSTEPSTRINGS; i++)
		{
			m_strStep = substepStrings[i].listItem;
			int type = substepStrings[i].stepID;
			if (type == m_nItemID){
				k=0;
				m_listStep.SetItemData(m_listStep.InsertString(k, m_strStep), substepStrings[i].searchType);
				k++;
			}
		}
	}
	
	UpdateData(FALSE);

	if (m_oldStep.IsEmpty()){//we are not editing a step
		m_listStep.SetCurSel(-1);//initialize list box with no selection
		m_listStep.SetCaretIndex(-1);
	}
	else
		m_listStep.SelectString(-1, m_oldStep);//select our old string which we will edit


	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}


void CStepDlg::OnDblclkList1() 
{
	OnOK();
	
}



void CStepDlg::OnBack() 
{
	EndDialog(BACK);
	
}


void CStepDlg::OnOK() 
{
	int curIndex = m_listStep.GetCurSel();
	if (curIndex == -1){
		theApp.DoWarningMessage("Please select an item", this);
		return;
	}

	m_nItemID = m_listStep.GetItemData(curIndex);

	CDialog::OnOK();
}
