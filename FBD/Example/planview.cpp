// PlanView.cpp : implementation file
//
// $Id: planview.cpp,v 1.1 2005/01/24 16:28:10 bvds Exp $

#include "stdafx.h"

#include "FBD.h"
#include "history.h"
#include "HelpIfc.h"
#include "Mainfrm.h"
#include "FBDDoc.h"
#include "FBDObj.h"
#include "FBDView.h"
#include "EQView.h"
#include "EXView.h"
#include "PlanView.h"


#include "PlanStrs.h"
#include "TabView.h"
#include "VariableDlg.h"
#include "StepDlg.h"
#include "DoneDlg.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CPlanView


IMPLEMENT_DYNCREATE(CPlanView, CTreeView)

CPlanView::CPlanView()
{}

CPlanView::~CPlanView()
{
	// m_items:  list of plan items serialized by the document
	// m_itemList:  list of plan items owned by the view
	//				for memory management purposes only
	//				only need to be deleted if view created
	POSITION pos = m_itemList.GetHeadPosition();
	while (pos != NULL)
	{//when in example edit, this view deletes all the planitems
		//when studying an example, these are deleted in DeleteNewPlanObjects
		//which is called in the document destructor
		//DeleteNewPlanObjects still deletes the one and only plan obj
		CPlanItem* pPlanItem = m_itemList.GetNext(pos);
		delete pPlanItem;
	}
	m_itemList.RemoveAll();

}	


BEGIN_MESSAGE_MAP(CPlanView, CTreeView)
	//{{AFX_MSG_MAP(CPlanView)
	ON_WM_CREATE()
	ON_WM_LBUTTONDBLCLK()
	ON_NOTIFY_REFLECT(TVN_SELCHANGED, OnSelchanged)
	ON_COMMAND(ID_PLAN_EDITTHISITEM, OnEdititem)
	ON_UPDATE_COMMAND_UI(ID_PLAN_EDITTHISITEM, OnUpdateEdititem)
	ON_WM_RBUTTONDOWN()
	ON_COMMAND(ID_PLAN_INSERTSUBITEM, OnInsertSubitem)
	ON_UPDATE_COMMAND_UI(ID_PLAN_INSERTSUBITEM, OnUpdateInsertSubitem)
	ON_COMMAND(ID_PLAN_DELETE, OnEditDelete)
	ON_COMMAND(ID_PLAN_ADDNEWGOAL, OnPlanAddnewgoal)
	ON_UPDATE_COMMAND_UI(ID_PLAN_ADDNEWGOAL, OnUpdatePlanAddnewgoal)
	ON_UPDATE_COMMAND_UI(ID_PLAN_DELETE, OnUpdateEditDelete)
	ON_WM_KILLFOCUS()
	ON_WM_LBUTTONDOWN()
	ON_WM_SETFOCUS()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPlanView drawing


/////////////////////////////////////////////////////////////////////////////
// CPlanView diagnostics

#ifdef _DEBUG
void CPlanView::AssertValid() const
{
	CTreeView::AssertValid();
}

void CPlanView::Dump(CDumpContext& dc) const
{
	CTreeView::Dump(dc);
}

CFBDDoc* CPlanView::GetDocument() // non-debug version is inline
{
	ASSERT(((CChildFrame*)GetParentFrame())->m_pDoc->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return ((CChildFrame*)GetParentFrame())->m_pDoc;
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPlanView message handlers

int CPlanView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{	//Creates the tree control, specifies the style, and sets the imagelist used to create labels
	GetDocument()->m_bPlanWnd = TRUE;
	lpCreateStruct->style |=TVS_HASLINES | TVS_LINESATROOT |TVS_HASBUTTONS|TVS_SHOWSELALWAYS;
	if (CTreeView::OnCreate(lpCreateStruct) == -1){//Create tree control
		TRACE("Failed to create tree control");
		return -1;
	}
	m_imgSteps.Create(IDR_IMAGE, 14, 1, RGB(255, 255, 255));//create image list
	GetTreeCtrl().SetImageList (&m_imgSteps, TVSIL_NORMAL);
/* Can't put in assertion until plan no longer in tabview
	CDocTemplate* pDocTemplate = ((CDocument*)GetDocument())->GetDocTemplate();
	ASSERT(pDocTemplate==theApp.m_ptmplExEdit);*/

	return 0;
}


void CPlanView::OnInitialUpdate() 
{
	CTreeView::OnInitialUpdate();
	if ((GetDocument()->m_plan.m_items.GetCount()==0))//plan object doesn't exist(no saved plan)
	{
		InsertInitialNode();
	}
	else
		PopulateTree();//rebuilds tree that was saved to a file	
}

////////////////////////////////////////////////////////////////
//CPlanView implementation functions
void CPlanView::InsertInitialNode()
{
	CString	substr = "Double click here to insert goal";
	//Inserts the first node of the tree, the toplevel goal
	//It is this node that we double click to begin the planning process
	CPlanItem* pPlanItem = new CPlanItem();//create new planitem
	m_itemList.AddTail(pPlanItem);
	pPlanItem->SetLevel(0);//level=0->item is a goal
	LONG lParam = (LONG)pPlanItem;
	//insert initial node
	HTREEITEM hItem = GetTreeCtrl().InsertItem(TVIF_PARAM |TVIF_STATE |TVIF_TEXT|
		TVIF_HANDLE| TVIF_IMAGE | TVIF_SELECTEDIMAGE,
			substr, 0, 0, TVIS_EXPANDED, TVIS_EXPANDED ,
				lParam, TVI_ROOT, TVI_LAST);
	GetTreeCtrl().Select(hItem, TVGN_CARET);
}

void CPlanView::PopulateTree()
{
	CFBDDoc* pDoc = GetDocument();
	
	POSITION pos = pDoc->m_plan.m_items.GetHeadPosition();//only one plan object-entire tree
	CPlanItem* pPlanItem = pDoc->m_plan.m_items.GetNext(pos);
	HTREEITEM hParent = TVI_ROOT;
	HTREEITEM hItem = InsertTreeItem(pPlanItem, hParent);
	//Map first plan item parent pointer to ROOT
	m_mapPlanitemToTreeitem.SetAt(pPlanItem->GetParent(), TVI_ROOT);
	while (pos !=NULL)
	{
		pPlanItem = pDoc->m_plan.m_items.GetNext(pos);
		if (!m_mapPlanitemToTreeitem.Lookup(pPlanItem->GetParent(), hParent))
		{//if plan item parent pointer not already mapped, parent is last item inserted
			hParent = hItem;
			m_mapPlanitemToTreeitem.SetAt(pPlanItem->GetParent(), hItem);
		}
		hItem = InsertTreeItem(pPlanItem, hParent);
	}
	GetTreeCtrl().Select(hItem, TVGN_CARET);
}

HTREEITEM CPlanView::InsertTreeItem(CPlanItem* pPlanItem, HTREEITEM hParent)
{
	m_itemList.AddTail(pPlanItem);//add to view managed list
	LONG lParam =(LONG) pPlanItem;//pointer to plan item stored in lParam
	int iImage = pPlanItem->GetImage();//item image
	CString pszText = pPlanItem->GetText();//item text

	HTREEITEM hItem = GetTreeCtrl().InsertItem(TVIF_PARAM |TVIF_STATE
		| TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE
			| TVIF_HANDLE, pszText, iImage,	iImage, TVIS_EXPANDED,
				TVIS_EXPANDED, lParam, hParent, TVI_LAST);

	return hItem;
}

void CPlanView::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
	HTREEITEM selItem = HitTest(point);
	if (selItem == NULL)//nothing selected
		return;
	OnEdititem();
}

HTREEITEM CPlanView::HitTest(CPoint point)
{
	HTREEITEM selItem=(GetTreeCtrl().HitTest(point));
	if (selItem == NULL){
		GetTreeCtrl().SelectItem(NULL);
		return NULL;
	}
	
//The tree control hittest signifies a hit when you click on the label, the text
//or the area extending from the text.  I have modified the hittest to only
//signify a hit only when clicking a point inside the rectangle surrounding the text
	CRect itemRect;
	BOOL bTextOnly = FALSE;
	GetTreeCtrl().GetItemRect(selItem,	&itemRect, bTextOnly);
	if ((itemRect.PtInRect(point) == NULL) || (bTextOnly = 0))
		return NULL;
	return selItem;

}

void CPlanView::OnEdititem() 
{
	if 	(GetTreeCtrl().ItemHasChildren(m_Selected))
	{	
		CString msg = "Changing this item will result in the deletion of all of its substeps";
		if (theApp.DoWarningMessage(msg, this, MB_OKCANCEL) != IDOK)
			return;	

		HTREEITEM selChild = GetTreeCtrl().GetChildItem(m_Selected);
		while (selChild != NULL){//delete child items
			GetTreeCtrl().DeleteItem(selChild);
			selChild = GetTreeCtrl().GetChildItem(m_Selected);
		}
		GetDocument()->SetModifiedFlag();

	}
	CPlanItem* pPlanItem = GetPlanItem(m_Selected);
	int level = pPlanItem->GetLevel();
	if (level == LEVEL_GOAL)
	{
		EditGoal(m_Selected);
		return;
	}
	m_oldText = GetTreeCtrl().GetItemText(m_Selected);
	HTREEITEM hItem = GetTreeCtrl().GetParentItem(m_Selected);
	GetTreeCtrl().DeleteItem(m_Selected);
	GetTreeCtrl().SelectItem(hItem);
	OnInsertSubitem();
	return;
}

void CPlanView::OnUpdateEdititem(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_Selected!=NULL);
}


void CPlanView::EditGoal(HTREEITEM selItem)
{
	//for the toplevel goal, the node already exists.  We simply need to change the
	//text of this node to reflect our toplevel goal which we created from a dialog box
	CFBDDoc* m_pDoc = GetDocument(); 


	CPlanItem* pPlanItem = GetPlanItem(m_Selected);
	
	
	HTREEITEM hParentItem = GetTreeCtrl().GetParentItem(selItem);
	

	CVarTypeDlg dlgVarType;
	if (dlgVarType.DoModal() != IDOK)
		return;

	CVariable* pVar = new CVariable();
	pVar->m_nType = dlgVarType.m_nType + ID_VARIABLE_ADDFIRST;

	CDialog* pDlg = pVar->GetPropertyDlg();
	((CDrawObjDlg*)pDlg)->m_bSought = TRUE;		

	if (pDlg->DoModal() != IDOK){ //dialog to specifiaclly define chosen type
		delete pVar;
		delete pDlg;
		return;
	}
	

	GetTreeCtrl().SetItemText(selItem, pVar->m_strDef);
	pPlanItem->SetText(GetTreeCtrl().GetItemText(selItem));

	delete pVar;
	delete pDlg;

	m_pDoc->SetModifiedFlag();
	return;
}

void CPlanView::InsertGoal(HTREEITEM selItem)
{	
	//We are inserting a new goal.
	CFBDDoc* pDoc = GetDocument();
	HTREEITEM hParent;//the parent of the new inserted goal
	HTREEITEM hInsertAfter = selItem;//item new goal is inserted after
	HTREEITEM selParent = GetTreeCtrl().GetParentItem(selItem);//parent of currently selected item
	//get image of selected item
	int nImage, nsImage;
	GetTreeCtrl().GetItemImage(selItem, nImage, nsImage);
	//get level of selected item
	CPlanItem* pPlanItem = GetPlanItem(m_Selected);
	int nParentlevel = pPlanItem->GetLevel();
	
	if ((nParentlevel == LEVEL_THIRDLAW_SUBGOAL)||(nParentlevel == LEVEL_UNKNOWN_SUBGOAL))
	{	//we are finding an equivalent quantity
		//or an unknown and we are inserting a subgoal
	/*	if (nParentlevel == LEVEL_THIRDLAW_SUBGOAL)
			dlgPlan.m_strHeader = "What is the reaction force?";
		else
			dlgPlan.m_strHeader = "What unknown quantity do you want to find?";
		*/
		//the selected item is a step if we are finding an eq. qty
		//the selected item is a substep if we are finding an unknown
		nImage = 1;//image set to 1 -> g
		hParent = selItem;
	
	}
	else//we are inserting another toplevel goal
	{
		nImage = 0;//image set to 0 -> G
		hParent = TVI_ROOT;
		hInsertAfter = TVI_LAST;
	}
	
	CVarTypeDlg dlgVarType;
	if (dlgVarType.DoModal() != IDOK)
		return;

	CVariable* pVar = new CVariable();
	pVar->m_nType = dlgVarType.m_nType + ID_VARIABLE_ADDFIRST;

	CDialog* pDlg = pVar->GetPropertyDlg();
	((CDrawObjDlg*)pDlg)->m_bSought = TRUE;		

	if (pDlg->DoModal() != IDOK){ //dialog to specifiaclly define chosen type
		delete pVar;
		delete pDlg;
		return;
	}
	CString strGoal = pVar->m_strDef;

	delete pVar;
	delete pDlg;

	pPlanItem = new CPlanItem();//create corresponding plan item
	m_itemList.AddTail(pPlanItem);//add to view list of plan items
	LONG lParam = (LONG)pPlanItem;
	pPlanItem->SetLevel(LEVEL_GOAL);//set item level
	

	// Get Text string for inserted goal from dialog
	pPlanItem->SetText(strGoal);

	// Insert new goal or subgoal
	HTREEITEM hItem = GetTreeCtrl().InsertItem(TVIF_PARAM |TVIF_STATE |TVIF_TEXT|
			TVIF_IMAGE | TVIF_SELECTEDIMAGE|TVIF_HANDLE,
			strGoal, nImage, nImage, TVIS_EXPANDED, TVIS_EXPANDED, lParam, hParent, hInsertAfter);
	
	pDoc->SetModifiedFlag();

	//select Inserted item and conitnue with planning process
	GetTreeCtrl().SelectItem(hItem);
	OnInsertSubitem();
}

void CPlanView::InsertSubstep(HTREEITEM selItem, int nParentLevel)
{	
	if ( (nParentLevel == LEVEL_THIRDLAW_SUBGOAL) ||
		(nParentLevel == LEVEL_UNKNOWN_SUBGOAL) ){
		InsertGoal(selItem);
		return;
	}

	LONG lParam = GetTreeCtrl().GetItemData(selItem);

	HTREEITEM insertAfter = TVI_LAST;
	HTREEITEM hParent = selItem;


	CPlanItem* pPlanItem = (CPlanItem*)lParam;

	int searchType = pPlanItem->GetSearchType();
	if ( (searchType == ID_FORCES) ||  (searchType == ID_SYSBODY)
			|| (nParentLevel == LEVEL_SUBSTEP) )
		return;

	
	CStepDlg dlgStep;
	dlgStep.m_oldStep = m_oldText;
	dlgStep.m_nItemID = searchType;// type of substep

	int nChildLevel; 
	if (nParentLevel == LEVEL_GOAL) 
		nChildLevel = LEVEL_PRINCIPLE;
	else
		nChildLevel = nParentLevel + 1;
	dlgStep.m_nLevel = nChildLevel;

	int dlgReply = dlgStep.DoModal();
	

	m_oldText="";
	if (dlgReply == IDCANCEL)//dialog cancelled
		return;
	else if (dlgReply == BACK){
		GetTreeCtrl().SelectItem(hParent);
		OnEdititem();
		return;
	}
	CString strSubStep;
	strSubStep = dlgStep.m_strStep;
	searchType =  dlgStep.m_nItemID;
	
	pPlanItem = new CPlanItem();//create corresponding plan item
	m_itemList.AddTail(pPlanItem);
	lParam = (LONG)pPlanItem;
	pPlanItem->SetLevel(nChildLevel);//set item level
	pPlanItem->SetSearchType(searchType);//get type of user action you are searching for
	pPlanItem->SetText(strSubStep);
	//insert substep
	HTREEITEM hItem = GetTreeCtrl().InsertItem(TVIF_PARAM |TVIF_STATE |TVIF_TEXT| TVIF_IMAGE |
		TVIF_SELECTEDIMAGE |TVIF_HANDLE, strSubStep, nChildLevel, 
			nChildLevel, TVIS_EXPANDED, TVIS_EXPANDED, lParam, 
				hParent, insertAfter);
	
	GetDocument()->SetModifiedFlag();
	GetTreeCtrl().SelectItem(hItem);
	if (searchType == UNKNOWNS)//we are finding an unknown
	{//this will become our new goal
		pPlanItem->SetLevel(LEVEL_UNKNOWN_SUBGOAL);
	}
	else if (searchType == ID_THIRD)//pPlanItem->m_itemSearchType
	{//Finding an equivalent quantity
		pPlanItem->SetLevel(LEVEL_THIRDLAW_SUBGOAL);
	}
	else if (searchType == ID_FORCE_EQN)
	{
		pPlanItem->SetLevel(LEVEL_STEP);
	}
	OnInsertSubitem();
}
	
	
	
	


void CPlanView::OnEditDelete() 
{

	if (m_Selected == NULL)//nothing selected
		return;
	LONG lParam = GetTreeCtrl().GetItemData(m_Selected);
	CPlanItem* pPlanItem = (CPlanItem*)lParam;
	int level = pPlanItem->GetLevel();
	if 	(GetTreeCtrl().ItemHasChildren(m_Selected))//no children
	{//if the item has children, we must warn that its deletion will delete its kids
		CString msg = "Deleting this item will result in the deletion of all substeps";
		if (theApp.DoWarningMessage(msg, this, MB_OKCANCEL) != IDOK)
			return;
		
	}
	
	GetTreeCtrl().DeleteItem(m_Selected);//this function deletes the item and its descendents
	GetDocument()->SetModifiedFlag();
	if (GetTreeCtrl().GetCount() == 0)//if we delete all the items, we return to the 
	{
		InsertInitialNode();	//initial state->one top level node to begin
	}							//the planning process
	
	return;
}




void CPlanView::OnSelchanged(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM thisItem = pNMTreeView->itemNew.hItem;
	if (thisItem == NULL){
		*pResult = 0;
		return;
	}
	m_Selected = thisItem;
	*pResult = 0;
}



void CPlanView::OnRButtonDown(UINT nFlags, CPoint point) 
{
	HTREEITEM selItem = HitTest(point);
	if (selItem == NULL)
		return;
	GetTreeCtrl().SelectItem(selItem);
	ClientToScreen(&point);
	LogEventf(EV_PLAN_MENU, "%d %d", point.x, point.y);
	// CG: This function was added by the Pop-up Menu component
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_PLANVIEW));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);

}


void CPlanView::OnInsertSubitem() 
{
	CPlanItem* pPlanItem = GetPlanItem(m_Selected);
	int nParentLevel = pPlanItem->GetLevel();
	
	InsertSubstep(m_Selected, nParentLevel);
	return;
}

void CPlanView::OnUpdateInsertSubitem(CCmdUI* pCmdUI) 
{
	CPlanItem* pPlanItem = GetPlanItem(m_Selected);
	int level = pPlanItem->GetLevel();
	int searchtype = pPlanItem->GetSearchType();
	if (m_Selected == NULL)
		pCmdUI->Enable(FALSE);
	else 
		pCmdUI->Enable( (level != 4) && (searchtype != ID_SYSBODY)
				&& (searchtype != ID_FORCES) );
	
	
}

//
// Update document data with plan items(tree items) and any links to equations or drawobjects
// Use before serializing document data.
//
void CPlanView::UpdateDoc()
{//plan information is put into a list right before saved
	//this info is extracted from the current tree.
	CFBDDoc* pDoc = GetDocument();
	pDoc->m_plan.m_items.RemoveAll();//fill m_items from current tree
	TRACE("Removing all from m_items\n");
	int count = pDoc->m_plan.m_items.GetCount();
	TRACE("Count is %d\n", count);

	int nImage, nSelImage;
	
	HTREEITEM hItem = GetTreeCtrl().GetRootItem();
	HTREEITEM hItemNext;
	
	while (hItem != NULL)
	{
		CPlanItem* pPlanItem = GetPlanItem(hItem);
		m_mapTreeitemToPlanitem[hItem] = pPlanItem;
		pPlanItem->SetText(GetTreeCtrl().GetItemText(hItem));
		GetTreeCtrl().GetItemImage(hItem, nImage, nSelImage);
		pPlanItem->SetImage(nImage);
	
		HTREEITEM hParent = GetTreeCtrl().GetParentItem(hItem);
		

		if (hParent == NULL)
			pPlanItem->SetParent(NULL);
		else {
			CPlanItem* pParentItem = m_mapTreeitemToPlanitem[hParent];
			ASSERT(pParentItem !=NULL);
			pPlanItem->SetParent(pParentItem);
		}
		
		pDoc->m_plan.m_items.AddTail(pPlanItem);
		hItemNext = GetNextExistingItem(hItem);
		hItem = hItemNext;
		
	}
}


HTREEITEM CPlanView::GetNextExistingItem(HTREEITEM hItem)
{
	HTREEITEM hNextItem;
	if (GetTreeCtrl().ItemHasChildren(hItem))
		hNextItem = GetTreeCtrl().GetNextItem(hItem, TVGN_CHILD);
	else{
		if (GetTreeCtrl().GetNextItem(hItem, TVGN_NEXT)==NULL)
			hNextItem = GetTreeCtrl().GetNextItem(hItem, TVGN_NEXTVISIBLE);
		else
			hNextItem = GetTreeCtrl().GetNextItem(hItem, TVGN_NEXT);
	}
	return hNextItem;
}


void CPlanView::OnPlanAddnewgoal() 
{
		HTREEITEM rootItem = GetTreeCtrl().GetRootItem();
		GetTreeCtrl().Select(rootItem, TVGN_CARET);
		
		CString text = GetTreeCtrl().GetItemText(rootItem);
		if (text[0] == 'D')
			EditGoal(rootItem);
		else
			InsertGoal(rootItem);
	
}

void CPlanView::OnUpdatePlanAddnewgoal(CCmdUI* pCmdUI) 
{
	// TODO: Add your command update UI handler code here
	
}


void CPlanView::OnUpdateEditDelete(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(m_Selected!=NULL);
}

void CPlanView::OnKillFocus(CWnd* pNewWnd) 
{
//	CTreeView::OnKillFocus(pNewWnd);
	
	//Eat this message so that the selected item does not change to the
	//unfocused color
}




CPlanItem* CPlanView::GetPlanItem(HTREEITEM hItem)
{
	LONG lParam = GetTreeCtrl().GetItemData(hItem);
	return (CPlanItem*)lParam;
}



