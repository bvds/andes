//CTEmplateDlg.cpp - implementation file

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "helpifc.h"
#include "EXPlanVw.h"
#include "EXView.h"
#include "DoneDlg.h"
#include "TemplateDlg.h"
#include "PlanStrs.h"


/////////////////////////////////////////////////////////////////////////////
// CTemplateDlg dialog
//
// The template dialog is a base class for the templates used in example
// study explanation.  They all should have 3 buttons: Back Submit Cancel
// Template dialogs should also have an array of statics and combos to 
// be filled in
//
/////////////////////////////////////////////////////////////////////////////

CTemplateDlg::CTemplateDlg(int id, CWnd* pParent /*=NULL*/)
	: CLogDialog(id, pParent)
{
	for (int i=0; i<4; i++)
		m_status[i]=statusUnknown;
	m_bExplained = FALSE;

}

IMPLEMENT_DYNAMIC(CTemplateDlg, CDialog)

BEGIN_MESSAGE_MAP(CTemplateDlg, CDialog)
	//{{AFX_MSG_MAP(CTemplateDlg)
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
	ON_CONTROL_RANGE(CBN_SELCHANGE, IDC_COMBO1, IDC_COMBO4, OnSelchangeCombo)
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CTemplateDlg message handlers
//
void CTemplateDlg::InitTemplate()
{
	ResetTemplate();

	CEXView* pView = theApp.GetEXView();
	CDrawObj* pExpObj = pView->GetEXObj();
	if (m_nPos == 0)
	{
		m_rule = pExpObj->m_pEXInfo->m_rule;
		m_strRule = pExpObj->m_pEXInfo->m_strRule;
	}
	else{
		m_rule = pExpObj->m_pEXInfo->m_rule1;
		m_strRule = pExpObj->m_pEXInfo->m_strRule1;
	}

	CString str = "Template for: " + m_strRule;
	SetWindowText(str);

	MoveDlgToBtmRight();

}

void CTemplateDlg::InsertCtrlStrs()
{
	return;//handled in base class
}

Status CTemplateDlg::CheckCombo(CLogCombo* pCbo)
{
	pCbo->Invalidate();
	int curSel = pCbo->GetCurSel();
	CString txt;
	CString idTS;
	int nSel = pCbo->GetCount();
	if (nSel==0)
		return statusUnknown;
	if (curSel == -1)
		return statusUnknown;
	pCbo->GetLBText(curSel, txt);
	
	for (int i=0; i<NTMPLSTRS; i++)
	{
		CString str = tmplStrs[i].catStrs;
		if (str == txt){
			idTS = tmplStrs[i].idTmplStrs;
			break;
		}
	
	}
	LPCTSTR pszResult =
		HelpSystemExecf( "(select-template |%s| %s)",
			STR2ARG(m_strRule),
			STR2ARG(idTS)
			);
	if (! pszResult)
		return statusUnknown;
	else return (strcmp(pszResult, "T") == 0) ?
								statusCorrect : statusError;
	
}

Status CTemplateDlg::GetStatus(CWnd* pWnd)
{
	int curID = pWnd->GetDlgCtrlID();
	return m_status[curID - IDC_COMBO1];
}

void CTemplateDlg::OnSelchangeCombo(UINT nID) 
{
	int nCombo = nID - IDC_COMBO1;
    ASSERT( nCombo >= 0 && nCombo < 10 );
	m_status[ nCombo ] = statusUnknown;
	
}

void CTemplateDlg::OnBack() 
{
	ShowWindow(SW_HIDE);
	//**********************************
	//go back to rule browser in plan
	CEXPlanVw* pView = theApp.GetEXPlanVw();
	pView->EnableWindow(TRUE);	
}


void CTemplateDlg::OnCancel()
{
	ShowWindow(SW_HIDE);
	CEXPlanVw* ppView = theApp.GetEXPlanVw();
	ppView->m_txtInstruct.EnableWindow(FALSE);
	ppView->m_txtInstruct.SetWindowText("Explaining role of this step in the solution");
	ppView->OnDone(TRUE);

	CLogDialog::OnCancel();
}

void CTemplateDlg::OnSubmit()
{
	return;//handled in base class
}

void CTemplateDlg::ResetTemplate()
{
	return;//handled in base class
}

HBRUSH CTemplateDlg::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	HBRUSH hbr = CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	if (nCtlColor != CTLCOLOR_EDIT)
		return hbr;
	// Set appropriate text foreground color
	switch (GetStatus(pWnd))
	{
	case statusCorrect:
		pDC->SetTextColor(RGB(0, 128, 0));	// 
		break;

	case statusError:
		pDC->SetTextColor(RGB(255, 0, 0));  // red
		break;

	default:
		TRACE("OnCtlColor: Bad status value, pWnd = %d\n", pWnd);
		// fall through ...
	case statusUnknown:
		break;								// leave in default color
	}

	
	// TODO: Return a different brush if the default is not desired
	return hbr;
}

BOOL CTemplateDlg::IsRightRule()
{
	LPCTSTR pszResult =
		HelpSystemExecf( "(set-up-template |%s|)", STR2ARG(m_strRule));
	if ((pszResult) && (strcmp(pszResult, "T") != 0)){
		ShowWindow(SW_HIDE);
		//if rule not submitted initially and now found to be incorrect
		//must go back to rule browser, can't build template from 
		//incorrect rule
		CEXPlanVw* pView = theApp.GetEXPlanVw();
		pView->OnSubmit(FALSE);
		pView->EnableWindow(TRUE);
		return FALSE;
	}
	return TRUE;
}

BOOL CTemplateDlg::IsAllCorrect(int nCboBoxes)
{
	for (int k=0; k < nCboBoxes; k++)
		m_status[k] = CheckCombo(m_boxes[k]);
	
	Invalidate();
	for (int i=0; i<nCboBoxes; i++){
		if ((m_status[i]!=statusCorrect)&&(m_boxes[i]->IsWindowVisible())){
			return FALSE;
		}
	}
	return TRUE;

}

void CTemplateDlg::UpdateBtns(BOOL bReset)
{
	if (bReset) {
		m_Submit.ShowWindow(SW_SHOW);
		m_Back.ShowWindow(SW_SHOW);
		m_Cancel.SetWindowText("Cancel");
	}
	else{
		//if not resetting, user all correct
		//only option is to close dialog
		m_Submit.ShowWindow(SW_HIDE);
		m_Back.ShowWindow(SW_HIDE);
		m_Cancel.SetWindowText("Close");
	}
	
	CRect rect;
	m_Submit.GetWindowRect(&rect);
	ScreenToClient(&rect);
	if (bReset){
		CRect oldRect;
		oldRect.top = rect.top;
		oldRect.bottom = rect.bottom;
		oldRect.left = rect.right + 20;
		oldRect.right = rect.right + 20 + rect.Width();
		m_Cancel.MoveWindow(&oldRect);//move back to right
	}
	else
		m_Cancel.MoveWindow(&rect);//move to middle
}

void CTemplateDlg::UpdateCombos(int nCboBoxes)
{
	CEXView* pView = theApp.GetEXView();
	CDrawObj* pExpObj = pView->GetEXObj();
	for (int n = 0; n < nCboBoxes; n++)
	{
		m_boxes[n]->ShowWindow(m_boxes[n]->GetCount()!=0);
	}
	if (m_bExplained){
		if (m_nPos == 0){
			for (int i = 0; i < nCboBoxes; i++)
			{
				m_boxes[i]->SetCurSel(pExpObj->m_pEXInfo->m_CrrctIntLst[i]);
			}
		}
		else
		{
			for (int i = 0; i < nCboBoxes; i++)
			{
				m_boxes[i]->SetCurSel(pExpObj->m_pEXInfo->m_CrrctIntLst1[i]);
			}
		}
		OnSubmit();
	}
}

void CTemplateDlg::UpdateEXInfo(int nCboBoxes)
{

	CEXView* pView = theApp.GetEXView();
	CDrawObj* pExpObj = pView->GetEXObj();
	CEXInfo* pInfo = pExpObj->GetEXInfo();
	if (m_nPos==0){
		for (int k=0; k < nCboBoxes; k++)
			pInfo->m_CrrctIntLst[k]=m_boxes[k]->GetCurSel();
	}
	else{
		for (int k=0; k < nCboBoxes; k++)
			pInfo->m_CrrctIntLst1[k]=m_boxes[k]->GetCurSel();
	}
	int nCount = pInfo->m_relations.GetCount();
	if (nCount == 0)
		return;
	POSITION pos = pInfo->m_relations.GetHeadPosition();
	while (pos != NULL){
		CDrawObj* pRelItem = pInfo->m_relations.GetNext(pos);
		ASSERT(pRelItem != NULL);
		CEXInfo* pRelInfo = pRelItem->GetEXInfo();
		int nMenuPos;
		if (nCount > 1){
			int nMenuPos = pRelItem->m_strId.Find("direction");
			if (((m_nPos==0)&&(nMenuPos==-1))||((m_nPos==1)&&(nMenuPos!=-1))){
				for (int k=0; k < nCboBoxes; k++){
					pRelInfo->m_CrrctIntLst[k] = m_boxes[k]->GetCurSel();
				}
			}
			else
				continue;
		}
		else if (nCount ==1){
			if (pExpObj->m_strId.Find("direction") == -1)
				nMenuPos = 0;
			else
				nMenuPos = 1;
			for (int k=0; k < nCboBoxes; k++){
				if (nMenuPos == 0)
					pRelInfo->m_CrrctIntLst[k] = m_boxes[k]->GetCurSel();
				else
					pRelInfo->m_CrrctIntLst1[k] = m_boxes[k]->GetCurSel();
			}
		}
	}
}

void CTemplateDlg::MoveDlgToBtmRight()
{
	CRect dlgRect, frmRect;
	GetWindowRect(&dlgRect);
	CFrameWnd* pWnd = GetParentFrame();
	pWnd->GetWindowRect(&frmRect);
	
	int dlgWidth = dlgRect.Width();
	int dlgHeight = dlgRect.Height();
	int pos = frmRect.Height()/2 - (dlgHeight/2);

	dlgRect.top = frmRect.bottom-dlgHeight-pos;
	dlgRect.bottom = frmRect.bottom-pos;
	dlgRect.right = frmRect.right;
	dlgRect.left = frmRect.right-dlgWidth;
	MoveWindow(&dlgRect);
}