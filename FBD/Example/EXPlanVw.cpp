// EXPlanVw.cpp : implementation file
//

#include "stdafx.h"

#include "fbd.h"
#include "history.h"
#include "FBDDoc.h"

#include "EXView.h"
#include "HelpIfc.h"

#include "Exp2Dlg.h"
#include "Exp4Dlg.h"
#include "ExpBdyDg.h"
#include "PlanStrs.h"

#include "EXPlanVw.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEXPlanVw

IMPLEMENT_DYNCREATE(CEXPlanVw, CFormView)

CEXPlanVw::CEXPlanVw()
	: CFormView(CEXPlanVw::IDD)
{
	//{{AFX_DATA_INIT(CEXPlanVw)
	m_bExplained = FALSE;
	m_bBrowser = FALSE;
	m_bPlan = FALSE;
	//}}AFX_DATA_INIT
}

CEXPlanVw::~CEXPlanVw()
{
	if ( theApp.m_wHelpFlags & fExample){
	//when example window destroyed, must destroy templates
		m_pTemplateDlg->DestroyWindow();
		m_pTemp2Dlg->DestroyWindow();
		m_pTemp4Dlg->DestroyWindow();
	}

}

void CEXPlanVw::DoDataExchange(CDataExchange* pDX)
{
	CFormView::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CEXPlanVw)
	DDX_Control(pDX, IDC_BROWSER_TYPE, m_txtBrowserType);
	DDX_Control(pDX, IDC_TEMPLATE, m_Template);
	DDX_Control(pDX, IDC_HEADING, m_txtHeading);
	DDX_Control(pDX, IDC_DIRECT, m_txtDirect);
	DDX_Control(pDX, IDC_DONE, m_Done);
	DDX_Control(pDX, IDC_SUBMIT, m_Submit);
	DDX_Control(pDX, IDC_TEXT, m_txtInstruct);
	DDX_Control(pDX, IDC_PLANTREE, m_ctrlPlan);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CEXPlanVw, CFormView)
	//{{AFX_MSG_MAP(CEXPlanVw)
	ON_NOTIFY(TVN_SELCHANGED, IDC_PLANTREE, OnSelchangedPlantree)
	ON_NOTIFY(NM_DBLCLK, IDC_PLANTREE, OnDblclkPlantree)
	ON_WM_ENABLE()
	ON_WM_CTLCOLOR()
	ON_NOTIFY(NM_CLICK, IDC_PLANTREE, OnClickPlantree)
	ON_BN_CLICKED(IDC_TEMPLATE, OnTemplate)
	ON_NOTIFY(TVN_SELCHANGING, IDC_PLANTREE, OnSelchangingPlantree)
	ON_WM_MOUSEMOVE()
	ON_NOTIFY(TVN_ITEMEXPANDED, IDC_PLANTREE, OnItemexpandedPlantree)
	ON_BN_CLICKED(IDC_SUBMIT, OnSubmit)
	ON_BN_CLICKED(IDC_DONE, OnDone)
	//}}AFX_MSG_MAP
	ON_UPDATE_COMMAND_UI(ID_PLAN_SUBMIT, OnUpdateSubmit)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEXPlanVw diagnostics

#ifdef _DEBUG
void CEXPlanVw::AssertValid() const
{
	CFormView::AssertValid();
}

void CEXPlanVw::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}

CFBDDoc* CEXPlanVw::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return (CFBDDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CEXPlanVw message handlers

void CEXPlanVw::OnInitialUpdate() 
{
	CFormView::OnInitialUpdate();
	if ( theApp.m_wHelpFlags & fExample){// allow example study help
		m_imgStatus.Create(IDR_STATUS, 13, 1, RGB(255, 255, 255));
		// create state image list (reflects status of plan step)
		m_ctrlPlan.SetImageList (&m_imgStatus, TVSIL_STATE);
		EnableWindow(FALSE);
		CFBDDoc* pDoc = GetDocument();
		// create rule template dialogs (modeless dialogs)
		m_pTemplateDlg = new CExplainBodyDlg(pDoc, this);
		if (!m_pTemplateDlg->Create(IDD_EXPLAIN_BODY, this))
			TRACE0("Failed to create rule template\n");
		m_pTemp2Dlg = new CExp2Dlg(pDoc, this);
		if (!m_pTemp2Dlg->Create(IDD_EXPLAIN_TWO, this))
			TRACE0("Failed to create rule two template\n");
		m_pTemp4Dlg = new CExp4Dlg(pDoc, this);
		if (!m_pTemp4Dlg->Create(IDD_EXPLAIN_FOUR, this))
			TRACE0("Failed to create rule two template\n");
	}
	else{//no example help, plan only
		m_Done.ShowWindow(SW_HIDE);//no done button
		m_Submit.ShowWindow(SW_HIDE);//no submit button
		m_ctrlPlan.EnableWindow(FALSE);//plan tree window disabled
		PopulatePlan();//insert plan
	}
}

void CEXPlanVw::PopulatePlan()
{
	m_txtBrowserType.SetWindowText("PLAN BROWSER");
	CFBDDoc* pDoc = GetDocument();
	// only one plan object-entire tree
	// these plan items are never saved in this mode,
	// they are deleted in the document destructor
	POSITION pos = pDoc->m_plan.m_items.GetHeadPosition();
	CPlanItem* pPlanItem = pDoc->m_plan.m_items.GetNext(pos);
	HTREEITEM hParent = TVI_ROOT;
	HTREEITEM hItem = TVI_ROOT;
//	Cristina doesn't want the goal so we shall not insert it.
//	HTREEITEM hItem = InsertTreeItem(pPlanItem, hParent);

	while (pos !=NULL)
	{
		pPlanItem = pDoc->m_plan.m_items.GetNext(pos);
		if (!m_mapPlanitemToTreeitem.Lookup(pPlanItem->GetParent(), hParent))
		{//if plan item parent pointer not already mapped, 
			//parent is last item inserted
			hParent = hItem;
			m_mapPlanitemToTreeitem.SetAt(pPlanItem->GetParent(), hItem);
		}
		hItem = InsertPlanItem(pPlanItem, hParent);
		
	}
	CollapseTree();
	m_mapPlanitemToTreeitem.RemoveAll();
	
}

HTREEITEM CEXPlanVw::InsertPlanItem(CPlanItem* pPlanItem, HTREEITEM hParent)
{
	CString pszText = pPlanItem->GetText();//item text
	int image = pPlanItem->GetImage();
	HTREEITEM hItem = m_ctrlPlan.InsertItem(pszText, 0, 0, hParent, TVI_LAST);
	if (image == 3){
		m_ctrlPlan.SetItemState(hItem, TVIS_BOLD, TVIS_BOLD);
	}
	return hItem;
}


void CEXPlanVw::OnSubmit(BOOL bDblClicked) 
{//if do not allow example study, this will never be called
	if (bDblClicked){
		if (m_bPlan)
			Logf("Plan-Submit-DoubleClick");
		else
			Logf("Rule-Submit-DoubleClick");
	}else{
		if (m_bPlan)
			Logf("Plan-Submit");
		else
			Logf("Rule-Submit");
	}
	if (m_ctrlPlan.GetSelectedItem() == NULL){
		theApp.DoWarningMessage("Please select a rule");
		return;
	}

	CString txt = m_ctrlPlan.GetItemText(m_Selected);//txt of sel item
	LPCTSTR pszResult;
	int rule ;
	if (m_bBrowser){//if showing rule browser
		LONG lParam = m_ctrlPlan.GetItemData(m_Selected);//get data
		rule = LOWORD(lParam);//rule id of selected rule
		pszResult = HelpSystemExecf( "(select-rule-name |%s|)", txt);
	}
	else if (m_bPlan){// if showing plan
		CString pos = GetItemPosition(m_Selected);//get position
		CString lispstr = LookupLispString(txt);//get lisp string
		pszResult =	HelpSystemExecf( "(submit-step-dlg %s (%s))", STR2ARG(lispstr), STR2ARG(pos));
	}
	
	ApplyStatus(m_Selected, pszResult);
	LONG lParam =m_ctrlPlan.GetItemData(m_Selected);
	int status = HIWORD(lParam);
	m_ctrlPlan.Invalidate();
	m_ctrlPlan.SetItemState(m_Selected, INDEXTOSTATEIMAGEMASK(status), TVIS_STATEIMAGEMASK);
	if (status != statusCorrect)
		return;
	///the rest will only happen if the student chose the correct answer
	m_Submit.ShowWindow(SW_HIDE);
	CEXView* pView = theApp.GetEXView();
	CDrawObj* pExpObj = pView->GetEXObj();
	CEXInfo* pInfo = pExpObj->GetEXInfo();
	if (m_bPlan){
		CString pos = GetItemPosition(m_Selected);
		pInfo->m_planItemPos = pos;
	}
	else if (m_bBrowser){
		m_Done.ShowWindow(SW_HIDE);
		if (m_nPos == 0){
			pInfo->m_strRule = txt;
			pInfo->m_rule = rule;
		}else{
			pInfo->m_strRule1 = txt;
			pInfo->m_rule1 = rule;
		}
	}
	int nCount = pInfo->m_relations.GetCount();
	if (nCount == 0)//no related objects, don't have to update
		return;
	POSITION pos = pInfo->m_relations.GetHeadPosition();
	while (pos != NULL){
		CDrawObj* pRelItem = pInfo->m_relations.GetNext(pos);
		ASSERT(pRelItem != NULL);
		if (m_bPlan){
			CString pos = GetItemPosition(m_Selected);
			pRelItem->m_pEXInfo->m_planItemPos = pos;
		}
		else if (m_bBrowser){
			int nMenuPos;
			if (nCount > 1){//uncovering tension, weight
				nMenuPos = pRelItem->m_strId.Find("direction");
				if (((m_nPos==0)&&(nMenuPos==-1))||((m_nPos==1)&&(nMenuPos!=-1))){
					pRelItem->m_pEXInfo->m_strRule = txt;
					pRelItem->m_pEXInfo->m_rule = rule;
				}
				else 
					continue;
			}
			else if (nCount == 1){//uncovering text object
				if (pExpObj->m_strId.Find("direction") == -1)
					nMenuPos = 0;
				else
					nMenuPos = 1;
				
				if (nMenuPos==0){
					pRelItem->m_pEXInfo->m_strRule = txt;
					pRelItem->m_pEXInfo->m_rule = rule;
				}else{
					pRelItem->m_pEXInfo->m_strRule1 = txt;
					pRelItem->m_pEXInfo->m_rule1 = rule;
				}
			}
		}
	}
}

void CEXPlanVw::OnSubmit()
{
	OnSubmit(FALSE);
}

void CEXPlanVw::OnDone()
{
	OnDone(FALSE);
}


void CEXPlanVw::CollapseTree()
{
	HTREEITEM hItem = m_ctrlPlan.GetRootItem();
	while (hItem != NULL){ //not collapsing kids if commented out
		m_ctrlPlan.Expand(hItem, TVE_COLLAPSE);
		hItem = GetNextExistingItem(hItem);
	}
}

void CEXPlanVw::OnUpdateSubmit(CCmdUI* pCmdUI) 
{

	pCmdUI->Enable(m_Selected!=NULL);
}


void CEXPlanVw::OnDone(BOOL bCancel) 
{// only called if allow example study help
	if (m_bPlan){
		if (bCancel)
			Logf("Plan-Done-Cancel");
		else
			Logf("Plan-Done");
	}
	else{
		if (bCancel)
			Logf("Browser-Done-Cancel");
		else
			Logf("Browser-Done");
	}
	m_bPlan = FALSE;
	m_bBrowser = FALSE;
	m_ctrlPlan.DeleteAllItems();//delete so can fill with plan or rules
	m_Submit.ShowWindow(SW_SHOWNA);
	m_Done.ShowWindow(SW_SHOWNA);
	m_Template.ShowWindow(SW_HIDE);
	EnableWindow(FALSE);
	
	CFBDDoc* pDoc = GetDocument();
	CMDIChildWnd* pChild = ((CMDIFrameWnd*)AfxGetApp()->m_pMainWnd)->MDIGetActive();
	ASSERT(pDoc != NULL);
	POSITION pos = pDoc->GetFirstViewPosition();
	while (pos != NULL){//reactivate example view
		CView* pView = pDoc->GetNextView(pos);
		if (pView->IsKindOf(RUNTIME_CLASS(CEXView))){
			pChild->SetActiveView(pView);
			CEXView* pEXView = (CEXView*)pView;
			pEXView->ResetView();
			pEXView->Invalidate();
		}
	}
		
}

HTREEITEM CEXPlanVw::GetNextExistingItem(HTREEITEM hItem)
{
	HTREEITEM hNextItem;
	if (m_ctrlPlan.ItemHasChildren(hItem))
		hNextItem = m_ctrlPlan.GetNextItem(hItem, TVGN_CHILD);
	else{
		if (m_ctrlPlan.GetNextItem(hItem, TVGN_NEXT)==NULL)
			hNextItem = m_ctrlPlan.GetNextItem(hItem, TVGN_NEXTVISIBLE);
		else
			hNextItem = m_ctrlPlan.GetNextItem(hItem, TVGN_NEXT);
	}
	return hNextItem;
}

CString CEXPlanVw::LookupLispString(CString listItem)
{
	CString item;
	for (int i=0; i<NSTEPSTRINGS; i++){
		item = stepStrings[i].listItem;
		if (strcmp(listItem, item)==0)
			return stepStrings[i].lispString;
	}
	for (int j=0; j<NMIDSTEPSTRINGS; j++){
		item = midstepStrings[j].listItem;
		if (strcmp(listItem, item)==0)
			return midstepStrings[j].lispString;
	}
	
	for (int l=0; l<NSUBSTEPSTRINGS; l++){
		item = substepStrings[l].listItem;
		if (strcmp(listItem, item)==0)
			return substepStrings[l].lispString;
	}
	return "";
}


void CEXPlanVw::OnSelchangedPlantree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM thisItem = pNMTreeView->itemNew.hItem;
	m_Selected = thisItem;
	
	*pResult = 0;
}

void CEXPlanVw::OnDblclkPlantree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	if (m_ctrlPlan.GetSelectedItem() == NULL){
		*pResult = 0;
		return;
	}
	else if (!(theApp.m_wHelpFlags & fExample)){//see if allow example study help
		*pResult = 0;
		return;
	}
	OnSubmit(TRUE);
	*pResult = 1;
	
}

void CEXPlanVw::ApplyStatus(HTREEITEM hItem, LPCSTR pszResult)
{
	LONG lParam = m_ctrlPlan.GetItemData(hItem);
	int status = HIWORD(lParam);
	int nRule = LOWORD(lParam);
	if (! pszResult)
		status = statusUnknown;
	else if (strcmp(pszResult, "T") == 0) 
		status = statusCorrect;	
	else
		status = statusError;
	lParam = MAKELONG(nRule, status);
	m_ctrlPlan.SetItemData(hItem, lParam);
	
}

void CEXPlanVw::OnEnable(BOOL bEnable) 
{
	CFormView::OnEnable(bEnable);
	if ((bEnable)&&(m_ctrlPlan.GetCount()==0)){
		ASSERT((theApp.m_wHelpFlags & fExample));
		if (m_bPlan){
			InsertPlan();
			m_txtHeading.SetWindowText("Plan for Newton's 2nd Law");
			CString str;
			str.LoadString(IDS_PLAN_DIRECT);
			m_txtDirect.SetWindowText(str);

		}
		else if (m_bBrowser){
			InsertRules();
			m_pTemplateDlg->m_bExplained = FALSE;
			m_pTemp2Dlg->m_bExplained = FALSE;
			m_pTemp4Dlg->m_bExplained = FALSE;
		}


	}
	
	m_Submit.EnableWindow(bEnable);
	m_Done.EnableWindow(bEnable);
	m_ctrlPlan.EnableWindow(bEnable);
	m_txtInstruct.EnableWindow(bEnable);
	m_txtDirect.EnableWindow(bEnable);
	m_txtHeading.EnableWindow(bEnable);
	m_txtBrowserType.EnableWindow(bEnable);
	
}

HBRUSH CEXPlanVw::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	HBRUSH hbr = CFormView::OnCtlColor(pDC, pWnd, nCtlColor);
	
	pDC->SetTextColor(RGB(128, 0, 128)); 	

	return hbr;
}

void CEXPlanVw::InsertPlan()
{
	if ((GetDocument()->m_plan.m_items.GetCount() != 0))//plan object doesn't exist(no saved plan)
		PopulatePlan();
	if (m_bExplained){
		CEXView* pView = theApp.GetEXView();
		CDrawObj* pExpObj = pView->GetEXObj();
		HTREEITEM hItem = GetItemFromPosition(pExpObj->m_pEXInfo->m_planItemPos);
		m_ctrlPlan.SelectItem(hItem);
		m_ctrlPlan.EnsureVisible(hItem);
		OnSubmit(FALSE);
	}
}


void CEXPlanVw::InsertRules()
{
	m_txtBrowserType.SetWindowText("RULE BROWSER");
	for (int i=0; i<NTOPRULES; i++){
		CString str = topRules[i].rule;
		HTREEITEM hItem = m_ctrlPlan.InsertItem(str, 0, 0, TVI_ROOT, TVI_LAST);
		m_ctrlPlan.SetItemState(hItem, TVIS_BOLD , TVIS_BOLD);
		for (int j=0; j<NRULES; j++){
			if (topRules[i].ruleID==rules[j].ruleID)
			{
				CString str= rules[j].rule;
				HTREEITEM hParent = hItem;
				if ((rules[j].nRule>=IDH_UNDEF)||(rules[j].nRule==ID_HEADER))
					hParent=hItem;
				else
					hParent=m_lastItem;
				HTREEITEM chItem = m_ctrlPlan.InsertItem(str, 0, 0, hParent, TVI_LAST);
				if (rules[j].nRule == ID_HEADER)
					m_ctrlPlan.SetItemState(chItem, TVIS_BOLD , TVIS_BOLD);
				LONG lParam = MAKELONG(rules[j].nRule, statusUnknown);
				m_ctrlPlan.SetItemData(chItem, lParam);
				if (hParent==hItem)
					m_lastItem = chItem;
			}
		}
	}
	m_Template.ShowWindow(SW_SHOW);
	CString strHead;
	if (m_bRuleExp){
		CEXView* pView = theApp.GetEXView();
		CDrawObj* pExpObj = pView->GetEXObj();
		if (m_nPos == 0){
			HTREEITEM hItem = FindItemFromId(pExpObj->m_pEXInfo->m_rule);
			m_ctrlPlan.SelectItem(hItem);
		}
		else if (m_nPos == 1){
			HTREEITEM hItem = FindItemFromId(pExpObj->m_pEXInfo->m_rule1);
			m_ctrlPlan.SelectItem(hItem);
		}
		OnSubmit(FALSE);
	}
}

void CEXPlanVw::OnClickPlantree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM thisItem = pNMTreeView->itemNew.hItem;
	if (thisItem == NULL){
		m_ctrlPlan.SelectItem(NULL);
		*pResult = 1;	
	}
	*pResult = 0;
}



void CEXPlanVw::OnTemplate() 
{
	Logf("Clicked-Template");
    if (m_ctrlPlan.GetSelectedItem() == NULL)
	{
		theApp.DoWarningMessage("Please select a rule");
		return;
	}
	LONG lParam = m_ctrlPlan.GetItemData(m_Selected);
	int rule = LOWORD(lParam);//the rule index held in the lParam
	CString txt = m_ctrlPlan.GetItemText(m_Selected);


	CFBDDoc* pDoc = (CFBDDoc*)theApp.GetDocument();
	CEXView* pView = theApp.GetEXView();
	CDrawObj* pExpObj = pView->GetEXObj();
	CEXInfo* pInfo = pExpObj->GetEXInfo();
	if (m_nPos == 0){
		pInfo->m_strRule = txt;
		pInfo->m_rule = rule;
	}else{
		pInfo->m_strRule1 = txt;
		pInfo->m_rule1 = rule;
	}
	int nCount = pInfo->m_relations.GetCount();

	POSITION pos = pInfo->m_relations.GetHeadPosition();
	while (pos != NULL){
		CDrawObj* pRelItem = pInfo->m_relations.GetNext(pos);
		ASSERT(pRelItem != NULL);
		int nMenuPos;
		if (nCount > 1){
		
			int nMenuPos = pRelItem->m_strId.Find("direction");
			if (((m_nPos==0)&&(nMenuPos==-1))||((m_nPos==1)&&(nMenuPos!=-1))){
				pRelItem->m_pEXInfo->m_strRule = txt;
				pRelItem->m_pEXInfo->m_rule = rule;
			}
		}

		else if (nCount == 1){
			if (pExpObj->m_strId.Find("direction") == -1)
				nMenuPos = 0;
			else
				nMenuPos = 1;
		
			if (nMenuPos==0){
				pRelItem->m_pEXInfo->m_strRule = txt;
				pRelItem->m_pEXInfo->m_rule = rule;
			}
			else{
				pRelItem->m_pEXInfo->m_strRule1 = txt;
				pRelItem->m_pEXInfo->m_rule1 = rule;
			}
		}
	}

	EnableWindow(FALSE);
	ShowTemplate(rule, m_nPos, FALSE);

}


CString CEXPlanVw::GetItemPosition(HTREEITEM hItem)
{
	HTREEITEM hsItem = m_ctrlPlan.GetRootItem();
	HTREEITEM chItem;
	HTREEITEM gchItem;
	ASSERT(hItem != NULL);
	CString pos = "0 0";
	if (hsItem == hItem)
		return pos;
	int i = 0;
	while (hsItem != NULL){
		chItem = m_ctrlPlan.GetChildItem(hsItem);
		while (chItem != NULL){
			if (chItem == hItem){
				CString str;
				str.Format("%s %d", pos, i);
				pos = str;
				return pos;
			}
			int j = 0;
			gchItem = m_ctrlPlan.GetChildItem(chItem);
			while (gchItem != NULL){
				if (gchItem == hItem){
					CString str;
					str.Format("%s %d %d", pos, i, j);
					pos = str;
					return pos;
				}
				gchItem = m_ctrlPlan.GetNextSiblingItem(gchItem);
				j++;
			}

			chItem = m_ctrlPlan.GetNextSiblingItem(chItem);
			i++;
		}
	}
	return pos;
}

void CEXPlanVw::OnSelchangingPlantree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	if (m_bPlan){
		*pResult = 0;
		return;
	}
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM thisItem = pNMTreeView->itemNew.hItem;
	if (thisItem != NULL){
		LONG lParam = m_ctrlPlan.GetItemData(thisItem);
		HTREEITEM hParent = m_ctrlPlan.GetParentItem(thisItem);
		int nRule = LOWORD(lParam);
		if ((nRule == ID_HEADER)||(hParent == NULL)||(nRule == ID_TEXT))
			*pResult = 1;
	}
	else
		*pResult = 0;
}

HTREEITEM CEXPlanVw::FindItemFromId(int ID)
{
	HTREEITEM hItem = m_ctrlPlan.GetRootItem();
	while (hItem != NULL){
		LONG lParam = m_ctrlPlan.GetItemData(hItem);
		int nRule = LOWORD(lParam);
		if (nRule == ID)
			return hItem;
		hItem = GetNextExistingItem(hItem);
	}
	return NULL;

}


void CEXPlanVw::OnMouseMove(UINT nFlags, CPoint point) 
{
	
	if ( (!( theApp.m_wHelpFlags & fExample))
			&&(!m_ctrlPlan.IsWindowEnabled()) ){
			CEXView* pView = theApp.GetEXView();
			pView->HideCtrlObjs();
			m_ctrlPlan.EnableWindow(TRUE);
			(void) HelpSystemSendf("(peek-plan)");
	}
	CFormView::OnMouseMove(nFlags, point);
}

void CEXPlanVw::OnItemexpandedPlantree(NMHDR* pNMHDR, LRESULT* pResult) 
{
	if ((theApp.m_wHelpFlags & fExample) && (!m_bPlan) ){
		*pResult = 0;
		return;
	}

	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	UINT action = pNMTreeView->action;
	HTREEITEM hItem = pNMTreeView->itemNew.hItem;
	CString txt = m_ctrlPlan.GetItemText(hItem);//txt of sel item
	CString lispstr = LookupLispString(txt);
	if (action == TVE_EXPAND)
		(void)HelpSystemSendf("(expand-step %s)", lispstr);
	else
		(void)HelpSystemSendf("(compress-step %s)", lispstr);
	*pResult = 0;
}



HTREEITEM CEXPlanVw::GetItemFromPosition(CString pos)
{
	HTREEITEM hItem = m_ctrlPlan.GetRootItem();
	for (int i = 5; i <= pos.GetLength(); i++){
		hItem = m_ctrlPlan.GetChildItem(hItem);
		int k = 0;
		CString str = "0";
		while (pos[i-1] != str ){
			hItem = m_ctrlPlan.GetNextSiblingItem(hItem);
			k++;
			str.Format("%d", k);
		}
		i++;
	}
	return hItem;

}


void CEXPlanVw::ShowTemplate(int rule, int pos, BOOL bExplained)
{
	if ((rule < 10 ) || ((rule >= 25 ) && (rule <=33)) ){
		m_pTemp2Dlg->m_nPos = pos;
		m_pTemp2Dlg->m_bExplained = bExplained;
		m_pTemp2Dlg->ShowWindow(SW_SHOW);
	}
	else if (((rule >= 17) && (rule <= 19)) || (rule >= 34)){
		m_pTemp4Dlg->m_nPos = pos;
		m_pTemp4Dlg->m_bExplained = bExplained;
		m_pTemp4Dlg->ShowWindow(SW_SHOW);
	}
	else{
		m_pTemplateDlg->m_nPos = pos;
		m_pTemplateDlg->m_bExplained = bExplained;
		m_pTemplateDlg->ShowWindow(SW_SHOW);
	}


}
