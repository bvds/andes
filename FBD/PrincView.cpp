// PrincView.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "history.h"
#include "helpifc.h"
#include "Mainfrm.h"	
#include "LawDialog.h"
#include "PrincView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPrincView

IMPLEMENT_DYNCREATE(CPrincView, CTreeView)

CPrincView::CPrincView()
{
}

CPrincView::~CPrincView()
{
}

BEGIN_MESSAGE_MAP(CPrincView, CTreeView)
	//{{AFX_MSG_MAP(CPrincView)
	ON_WM_CREATE()
	ON_WM_CONTEXTMENU()
	ON_COMMAND(ID_PRINCIPLE_ADD, OnPrincipleAdd)
	ON_COMMAND(ID_PRINCIPLE_DELETE, OnPrincipleDelete)
	ON_UPDATE_COMMAND_UI(ID_PRINCIPLE_DELETE, OnUpdatePrincipleDelete)
	ON_WM_RBUTTONDOWN()
	ON_WM_LBUTTONDOWN()
	ON_WM_KEYDOWN()
	ON_NOTIFY_REFLECT(TVN_SELCHANGING, OnSelchanging)
	ON_NOTIFY_REFLECT(TVN_ITEMEXPANDING, OnItemexpanding)
	ON_COMMAND(ID_HELP_WHATSWRONG, OnHelpWhatswrong)
	ON_UPDATE_COMMAND_UI(ID_HELP_WHATSWRONG, OnUpdateHelpWhatswrong)
	ON_COMMAND(ID_PRINCIPLE_MODIFY, OnPrincipleModify)
	ON_UPDATE_COMMAND_UI(ID_PRINCIPLE_MODIFY, OnUpdatePrincipleModify)
	ON_WM_LBUTTONDBLCLK()
	//}}AFX_MSG_MAP

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPrincView drawing

void CPrincView::OnDraw(CDC* pDC)
{
	CFBDDoc* pDoc = GetDocument();
	// TODO: add draw code here
}

void CPrincView::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	// TODO: Add your specialized code here and/or call the base class
	switch (lHint) {
	case HINT_CHKCHANGE_PRINC: {	// also used for status change
		ASSERT_KINDOF(CPrincObj, pHint);
		CPrincObj* pPrinc = (CPrincObj*) pHint;
		HTREEITEM hItem = FindPrincItem(pPrinc);
		if (hItem) {
			SyncCheckState(hItem, pPrinc);
		}
	} break;

	case HINT_UPDATE_PRINC: {		// some change in princobj
		ASSERT_KINDOF(CPrincObj, pHint);
		CPrincObj* pPrinc = (CPrincObj*) pHint;
		HTREEITEM hItem = FindPrincItem(pPrinc);
		if (hItem == NULL) return;
		// principle may have new subitems if changed law. For now,
		// just delete existing tree item and add princobj again 
		// This also takes care of updating child status if necessary
		// Note must add at same place (after same prev item).
		HTREEITEM hPrev = GetTreeCtrl().GetPrevSiblingItem(hItem);
		if (hPrev == NULL) hPrev = TVI_LAST;
		GetTreeCtrl().DeleteItem(hItem);
		AddPrincItem(pPrinc, TVI_ROOT, hPrev);
	}break;
	// Currently ignore all other update hints to avoid flickering on changes
	// that don't affect our presentation.
	default:
		break;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CPrincView diagnostics

#ifdef _DEBUG
void CPrincView::AssertValid() const
{
	CTreeView::AssertValid();
}

void CPrincView::Dump(CDumpContext& dc) const
{
	CTreeView::Dump(dc);
}

CFBDDoc* CPrincView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return (CFBDDoc*)m_pDocument;
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPrincView message handlers

int CPrincView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	// Tried TVS_CHECKBOXES style, in which control is supposed to create checkbox
	// state images and manage them correctly on clicks, spacebar presses.
	// But it seemed to have bugs redrawing when we initialized check state
	// if we didn't set any images of our own. Worse, we want to use different
	// images for different colors to show status, and the control's automatic toggling
	// of checkstate on clicks evidently cycles through available state images.
	// So we don't use CHECKBOXES at all, just do it all ourselves w/state images. 
	lpCreateStruct->style |=  TVS_DISABLEDRAGDROP;
	if (CTreeView::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	// set our state images, see below for list:
	m_imgState.Create(IDB_CHECKS, 17, 1, RGB(128, 128, 128)); 
	GetTreeCtrl().SetImageList (&m_imgState, TVSIL_STATE);

	return 0;
}

// 
// Helpers for state image mapping:
// We must map between 12 different state image indices used in the control and
// [type (parent or leaf), check, status color] from PrincItem.
// Note order of status enum is Unknown(black), Correct(green), Error(red).
// Order of images as 2 x 2 x 3 array is used in state index calculation below.
//
enum CheckState 			// must match order in image list bitmap resource
{
	NoCheckBox,				// element 0 unused, 0 index means no image
	//
	// states for user checkable leaf items:
	//
	UncheckedBlack,		
	UncheckedGreen,
	UncheckedRed,
	CheckedBlack,		
	CheckedGreen,		
	CheckedRed,		
	//
	// states for parent items derived from state of children steps
	// These checkboxes have grey insides to show they are read-only.
	//
	UncheckedBlackDisabled,		
	UncheckedGreenDisabled,
	UncheckedRedDisabled,
	CheckedBlackDisabled,		
	CheckedGreenDisabled,	
	CheckedRedDisabled,
	
	MAXSTATE = CheckedRedDisabled,		// highest legal value
};

static BOOL IsCheckedState(int state)
{
	return  (state >= CheckedBlack && state <= CheckedGreen)
		|| (state >= CheckedBlackDisabled && state <= CheckedRedDisabled);	
}

static BOOL IsDisabledState(int state)
{ return state >= UncheckedBlackDisabled && state <= CheckedRedDisabled; }

CheckState static GetStateIndex(CPrincItem* pItem)
{
	// child is disabled and uncheckable if parent is bad
	if (pItem->m_pParent && pItem->m_pParent->m_status == statusError)
		return UncheckedBlackDisabled;

	BOOL bParent = pItem->HasSubItems();

	// after unused 0, state is at offset into 2 x 2 x 3 array
	// if checked, used check status, else main status
	int nStatusOffset = pItem->m_bChecked ? pItem->m_checkStatus : pItem->m_status;
	int nState = 1 + (bParent * 6) + (pItem->m_bChecked * 3) + nStatusOffset;
	ASSERT(nState >= 1 && nState <= MAXSTATE);
	return (CheckState) nState;
}

// Set tree item image (including color) to match state of pItem
void CPrincView::SyncCheckState(HTREEITEM hItem, CPrincItem *pItem)
{
	CheckState state = GetStateIndex(pItem);
	GetTreeCtrl().SetItemState(hItem,INDEXTOSTATEIMAGEMASK(state), TVIS_STATEIMAGEMASK);
}

#define STATEIMAGEMASKTOINDEX(i) ((i) >> 12)	// extract index from state bit mask

// Determine if checked only (any color)
BOOL CPrincView::ItemIsChecked(HTREEITEM hItem)
{
	return IsCheckedState(STATEIMAGEMASKTOINDEX(GetTreeCtrl().GetItemState(hItem,
																TVIS_STATEIMAGEMASK)));
}

BOOL CPrincView::ItemIsDisabled(HTREEITEM hItem)
{
	return IsDisabledState(STATEIMAGEMASKTOINDEX(GetTreeCtrl().GetItemState(hItem,
																TVIS_STATEIMAGEMASK)));
}

void CPrincView::OnInitialUpdate() 
{
	CTreeView::OnInitialUpdate();
	
	// Use dummy root item as title. Note no PrincItem attached to this one
	m_hTitleItem = GetTreeCtrl().InsertItem("Principles:");
	// Title is bold and has no checkbox state image.
	GetTreeCtrl().SetItemState(m_hTitleItem, TVIS_BOLD |INDEXTOSTATEIMAGEMASK(0), 
								TVIS_BOLD | TVIS_STATEIMAGEMASK);

	// Populate tree from document. 
	POSITION pos = GetDocument()->m_principles.GetHeadPosition();
	while (pos != NULL){
		CPrincObj* pPrinc = GetDocument()->m_principles.GetNext(pos);
		AddPrincItem(pPrinc);
	}
}

// Add an item and all its subitems to tree under given parent
void CPrincView::AddPrincItem(CPrincItem *pItem, HTREEITEM hParent /*=TVI_ROOT*/,
							  HTREEITEM hInsertAfter /* = TVI_LAST*/)
{
	CString strItem = pItem->GetDisplayString(); // text to display for item

	// Add tree item under parent, with back pointer to PrincItem in ItemData.
	HTREEITEM hItem = GetTreeCtrl().InsertItem(strItem, hParent, hInsertAfter);
	GetTreeCtrl().SetItemData(hItem, (DWORD)pItem);

	// set initial check state from PrincItem
	// GetTreeCtrl().SetCheck(hItem, pItem->m_bChecked);
	SyncCheckState(hItem, pItem);
	// ASSERT(GetTreeCtrl().GetCheck(hItem) == pItem->m_bChecked); 

	// Recursively add all subtrees of this item
	if (pItem->HasSubItems()) {
		POSITION posChild = pItem->m_subItems.GetHeadPosition();
		while (posChild != NULL) {
			CPrincItem* pChild = pItem->m_subItems.GetNext(posChild);
			AddPrincItem(pChild, hItem);
		}

		// Make sure parent is expanded so subtree is visible.
		GetTreeCtrl().Expand(hItem, TVE_EXPAND);
	}
}

void CPrincView::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	LogEventf(EV_PRINCIPLE_MENU, "%d %d", point.x, point.y);
	// CG: This function was added by the Pop-up Menu component
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_PRINCVIEW));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);
}

void CPrincView::OnRButtonDown(UINT nFlags, CPoint point) 
{
	HTREEITEM selItem = GetTreeCtrl().HitTest(point);
	// Can still show menu if no selection 
	// if (selItem == NULL)
	// 	return;
	if (selItem != m_hTitleItem) // don't select dummy title item
		GetTreeCtrl().SelectItem(selItem);

	ClientToScreen(&point);
	OnContextMenu(this, point);
}

// for testing only:
static int nHelpCalls = 0;	// number of simulated help system calls

void CPrincView::OnPrincipleAdd() 
{
	LogEventf(EV_PRINCIPLE_NEW, "");

	// create a new principle/stage pair. Note body, law undefined (empty strings).
	CPrincObj* pPrinc = new CPrincObj(TRUE); // T=>create associated stage

	// Add it to the document -- mainly to generate a new stage id
	// Note principle must be added before we notify other views (i.e. HiLevelVw)
	// because adding the principle automatically adds the stage and builds template
	GetDocument()->AddPrinciple(pPrinc);//Add to list of stages->Modified flag set here
	
	// Update this view w/new princ: Just add new dummy princ item to tree. 
	// Then we'll have an item to modify on update when properties are set in dialog
	AddPrincItem(pPrinc);

	// Redraw other views with new principle i.e. hi-level plan
	GetDocument()->UpdateAllViews(this, HINT_ADD_PRINC, pPrinc);

	// Show property dialog to let user define its properties
	CLawDialog dlg(pPrinc);
	if (dlg.DoModalWithHints() != IDOK) // cancelled defining
	{	
		// remove the dummy tree item
		HTREEITEM hItem = FindPrincItem(pPrinc);
		if (hItem)
			GetTreeCtrl().DeleteItem(hItem);

		// notify other views its going away
		GetDocument()->UpdateAllViews(this, HINT_DELETE_PRINC, pPrinc);
		GetDocument()->RemovePrinciple(pPrinc);//remove this stage

		// notify help system (may have been submitted on an OK w/errs in dialog)
		HelpSystemSendf("(delete-plan-stage %s)", pPrinc->GetStageId());

		// and free it up
		delete pPrinc;//delete this principle -> stage's destructor deletes its items 
						//item's destructor deletes its controls
		return;
	}
	// else dialog should have updated princobj and its status on OK.

/*	No longer necessary, dealt with in HINT_UPDATE_PRINC handler
	// If parent principle is wrong, update children as well (disables them).
	if (pPrinc->m_status == statusError && pPrinc->HasSubItems()) {
		POSITION pos = pPrinc->m_subItems.GetHeadPosition();
		while (pos != NULL) {
			CPrincItem* pChild = pPrinc->m_subItems.GetNext(pos);
			HTREEITEM hChildItem = FindPrincItem(pChild);
			if (hChildItem) 
				SyncCheckState(hChildItem, pChild);
		}
	} */
}

BOOL CPrincView::HaveSelectedPrinciple()
{
	HTREEITEM hItem = GetTreeCtrl().GetSelectedItem();
	CPrincItem* pItem = hItem ? (CPrincItem*) GetTreeCtrl().GetItemData(hItem) : NULL;
	//	Ignore title and subsidiary child items
	return (hItem && hItem != m_hTitleItem &&
		    pItem && !pItem->IsKindOf(RUNTIME_CLASS(CPrincStep)) );
}

void CPrincView::OnUpdatePrincipleDelete(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(HaveSelectedPrinciple());
}

void CPrincView::OnPrincipleDelete() 
{
	HTREEITEM hItem = GetTreeCtrl().GetSelectedItem();
	if (hItem == NULL || hItem == m_hTitleItem) // don't delete title item (shouldn't happen)
		return;

	CPrincItem* pItem = (CPrincItem*) GetTreeCtrl().GetItemData(hItem);

	CString strItemArg = ItemToArg(pItem);
	LogEventf(EV_PRINCIPLE_DELETE, "%s", strItemArg);
	
	if (pItem != NULL)
	{  
		// make sure selection is top-level princ, not a substep
		if (!pItem->IsKindOf(RUNTIME_CLASS(CPrincObj))) // shouldn't happen
			return;
		CPrincObj* pPrinc = (CPrincObj*) pItem;
		ASSERT_VALID(pPrinc);
		
		// Remove it from document
		GetDocument()->UpdateAllViews(this, HINT_DELETE_PRINC, pPrinc);
		GetDocument()->RemovePrinciple(pPrinc);//remove this stage

		// notify help system
		HelpSystemSendf("(delete-plan-stage %s)", pPrinc->GetStageId());

		// and free it up
		delete pPrinc;//delete this principle -> stage's destructor deletes its items 
						//item's destructor deletes its controls
	} else {
		CString strText = GetTreeCtrl().GetItemText(hItem);
		TRACE("On Delete tree item \"%s\": no PrincItem!\n", strText);
	}

	GetTreeCtrl().DeleteItem(hItem);
}


void CPrincView::OnPrincipleModify() 
{
	HTREEITEM hItem = GetTreeCtrl().GetSelectedItem();
	if (hItem == NULL || hItem == m_hTitleItem) // shouldn't happen
		return;
	CPrincObj* pPrinc = (CPrincObj*) GetTreeCtrl().GetItemData(hItem);
	if (pPrinc == NULL) return;					// shouldn't happen
	// make sure it's top-level princ, not a substep
	if (! pPrinc->IsKindOf(RUNTIME_CLASS(CPrincObj))) // shouldn't happen
		return;
	ASSERT_VALID(pPrinc);

	CString strItemArg = ItemToArg(pPrinc);
	LogEventf(EV_PRINCIPLE_MODIFY, "%s", strItemArg);
	
	CLawDialog dlg(pPrinc);
	dlg.DoModalWithHints(); 
}

void CPrincView::OnUpdatePrincipleModify(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(HaveSelectedPrinciple());
}

//
// For checking/unchecking steps as done
//

// Do updates on change in check state (check the checkstate)
void CPrincView::OnCheckStateChange(HTREEITEM hItem)
{
	if (hItem == NULL) return;	
	BOOL bCheckedNow = ItemIsChecked(hItem);

	CPrincItem* pItem = (CPrincItem*) GetTreeCtrl().GetItemData(hItem);
	if (!pItem) return;

	ASSERT_KINDOF(CPrincItem, pItem);
	ASSERT_VALID(pItem);
		
	// if it's a child substep, update the parent item 
	if (pItem->m_pParent)
		pItem->m_pParent->OnChildStateChange();

	// notify other views of change.
	// currently only broadcast updates on principle changes, not substeps
	if (pItem->IsKindOf(RUNTIME_CLASS(CPrincObj)))
		GetDocument()->UpdateAllViews(this, HINT_CHKCHANGE_PRINC, pItem);

	// If checked, get status from the help system
	if (pItem->m_bChecked) {
		CString strStageId = pItem->GetStageId();
		CString strStepId;	// leave empty for parents
		if (pItem->IsKindOf(RUNTIME_CLASS(CPrincStep)))
			strStepId = ((CPrincStep*)pItem)->GetStepId();

		LPCSTR pszResult = HelpSystemExecf("(substep-finished %s %s)",
				STR2ARG(strStepId), strStageId);
		CStringList Errs;	// unused
		CCheckedObj::ApplyStatus(pszResult, pItem->m_checkStatus, Errs);
			
		// must update to show new status
		SyncCheckState(hItem, pItem);

		// if it's a child substep, update the parent item 
		if (pItem->m_pParent)
			pItem->m_pParent->OnChildStateChange();
	}
}

void CPrincView::ToggleCheckState(HTREEITEM hItem)
{
	// Find the old state of the PrincItem
	CPrincItem* pItem = (CPrincItem*) GetTreeCtrl().GetItemData(hItem);
	if (pItem == NULL) return;
	// disallow changes on read-only parent items (shouldn't happen)
	if (pItem->HasSubItems()) return;
	
	// Toggle the check
	pItem->m_bChecked = ! pItem->m_bChecked;
	TRACE("%s \"%s\"\n", pItem->m_bChecked? "Checked" : "Unchecked", pItem->GetDisplayString());
	GetDocument()->SetModifiedFlag();
	// check status is now unknown
	pItem->m_checkStatus = statusUnknown;

	// update item in tree control to show new check state
	SyncCheckState(hItem, pItem);

	// Log the change
	CString strItemArg = ItemToArg(pItem);
	LogEventf(EV_PRINCIPLE_SETCHECK, "%s %d", strItemArg, pItem->m_bChecked);

	// and trigger updates tied to checkstate change
	OnCheckStateChange(hItem);
	return;
}

void CPrincView::OnLButtonDown(UINT nFlags, CPoint point) 
{
	UINT uFlags=0;
	HTREEITEM hHitItem = GetTreeCtrl().HitTest(point,&uFlags);
	if( uFlags & TVHT_ONITEMSTATEICON) {
		if (!ItemIsDisabled(hHitItem))	// disallow for disabled items
			ToggleCheckState(hHitItem);
		return;
	}
	
	// else let tree control process mouse click
	CTreeView::OnLButtonDown(nFlags, point);
}

// can also toggle checkstate with space bar if selected:
void CPrincView::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if( nChar == VK_SPACE ){
		HTREEITEM hSelItem = GetTreeCtrl().GetSelectedItem();
		if (!ItemIsDisabled(hSelItem))	// disallow for disabled items
			ToggleCheckState(hSelItem);
		return;
	}
		
	// else let tree control process keypress
	CTreeView::OnKeyDown(nChar, nRepCnt, nFlags);
}

void CPrincView::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
	// see if dblclick hit selected item, and if it's
	// a top-level principle
	UINT uFlags=0;
	HTREEITEM hHitItem = GetTreeCtrl().HitTest(point,&uFlags);
	if (hHitItem == GetTreeCtrl().GetSelectedItem() && HaveSelectedPrinciple()) {
		OnPrincipleModify();
		return;
	}
	// else
	CTreeView::OnLButtonDblClk(nFlags, point);
}

void CPrincView::OnSelchanging(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	HTREEITEM hItem = pNMTreeView->itemNew.hItem; // where sel is going

	// don't let selection go to title item
	if (hItem == m_hTitleItem) {
		*pResult = TRUE;	// suppresses change
		return;
	}

	// Log change
	// !!! need invertible item id for princitem arg in log
	CPrincItem* pItem = hItem ?  (CPrincItem*)GetTreeCtrl().GetItemData(hItem) : NULL;
	CString strItemArg = ItemToArg(pItem);
	LogEventf(EV_PRINCIPLE_SELECT, "%s", strItemArg);

	*pResult = 0;
}

void CPrincView::OnItemexpanding(NMHDR* pNMHDR, LRESULT* pResult) 
{
	NM_TREEVIEW* pNMTreeView = (NM_TREEVIEW*)pNMHDR;
	
	// Disallow any actions that could contract node
	if (pNMTreeView->action == TVE_COLLAPSE || 
		pNMTreeView->action == TVE_COLLAPSERESET ||
		pNMTreeView->action == TVE_TOGGLE) {
		*pResult = TRUE; // suppresses change
		return;
	} 
		
	*pResult = 0;
}


// generic routine to find tree item by item data. Copied from CodeGuru site.
HTREEITEM CPrincView::FindItemData(DWORD dwData, HTREEITEM hItem /* = TVI_ROOT */)
{
	if(!hItem)
		return NULL;
	
	// see if it's on this item
	// AW-added: seems to bomb on TVI_ROOT so inserted test
	if(hItem != TVI_ROOT && GetTreeCtrl().GetItemData(hItem) == dwData) 
		return hItem;

	HTREEITEM hRet = NULL;
	HTREEITEM hChild = GetTreeCtrl().GetChildItem(hItem);
	if(hChild) 
		hRet = FindItemData(dwData, hChild);

	if(hRet == NULL) {
		HTREEITEM hSibling = GetTreeCtrl().GetNextSiblingItem(hItem);
		if(hSibling)
			hRet = FindItemData(dwData, hSibling);
	}

	return hRet;
}



void CPrincView::OnHelpWhatswrong() 
{
	HTREEITEM hItem = GetTreeCtrl().GetSelectedItem();
	if (hItem == NULL) return;
	CPrincItem* pItem = (CPrincItem*) GetTreeCtrl().GetItemData(hItem);
	if (! pItem) return;

	CString strItemArg = ItemToArg(pItem);
	LogEventf(EV_PRINCIPLE_WHATSWRONG, "");

	CString strStageId = pItem->GetStageId();
	CString strStepId = "NIL";
	if (pItem->IsKindOf(RUNTIME_CLASS(CPrincStep)))
		strStepId = ((CPrincStep*)pItem)->GetStepId();
	LPCTSTR pszResult = HelpSystemExecf("(why-wrong-plan-item %s substep %s",
										 strStageId, strStepId);

	theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong);
}

void CPrincView::OnUpdateHelpWhatswrong(CCmdUI* pCmdUI) 
{
	CPrincItem* pItem = NULL;
	HTREEITEM hItem = GetTreeCtrl().GetSelectedItem();
	if (hItem != NULL)
		pItem = (CPrincItem*) GetTreeCtrl().GetItemData(hItem);
	pCmdUI->Enable(pItem && 
		           (pItem->m_status == statusError 
					|| (pItem->m_bChecked && pItem->m_checkStatus == statusError)));	
}

//------------------------------------------------------------------------------
//
// For log,demo record/playback -- need to write/parse textual item references
//
//------------------------------------------------------------------------------
static const char szNullArg[] = "null";

CString CPrincView::ItemToArg(CPrincItem *pItem)
{
	if (! pItem)		// OK, used in case set no selection, e.g.
		return szNullArg;

	CString strResult;
	if (pItem->IsSubItem())
	{
		CString strParentRef = ItemToArg(pItem->m_pParent);
	
		// combine parent ref with subitem id
		ASSERT_KINDOF(CPrincStep, pItem);
		CString strSubItem = ((CPrincStep*)pItem)->GetStepId();
		strResult.Format("%s/%s", strParentRef, strSubItem);
	} else // top-level item
		strResult = pItem->GetStageId();

	return strResult;
}

// param should have only single item-ref arg, nothing after
CPrincItem* CPrincView::ArgToItem(LPCTSTR pszArg)
{
	CString strArg(pszArg);
	if (strArg == szNullArg) return NULL;

	// split argument into stage and subitem id.
	CString strStageId, strStepId;
	int nSepPos = strArg.Find('/');
   	if (nSepPos != -1) {		// has command
   		strStageId = strArg.Left(nSepPos);
   		strStepId = strArg.Mid(nSepPos + 1); 
   	}else
   		strStageId = strArg;

	// search list to find principle
	POSITION posPrinc = GetDocument()->m_principles.GetHeadPosition();
	while (posPrinc != NULL) {
		CPrincObj* pPrinc = GetDocument()->m_principles.GetNext(posPrinc);
		if (pPrinc->GetStageId() == strStageId) {
			// found principle, see if have subitem to match
			if (strStepId.IsEmpty())
				return pPrinc;		// we're done

			// else search for matching subitem
			POSITION posSubItem = pPrinc->m_subItems.GetHeadPosition();
			while (posSubItem != NULL) {
				CPrincItem* pItem = pPrinc->m_subItems.GetNext(posSubItem);
				ASSERT_KINDOF(CPrincStep, pItem);
				if (((CPrincStep*)pItem)->GetStepId() == strStepId)
					return pItem;
			}
		}
	}

	TRACE("CPrincView:: item \"%s\" not found!\n", pszArg); 
	return NULL;
}

// Select given item. May be NULL for no selection.
void CPrincView::SelectItem(CPrincItem *pItem)
{
	//find it in tree control
	HTREEITEM hItem = FindPrincItem(pItem);	
	if (pItem && ! hItem) {
		TRACE("PrincView::SelectItem -- Item not found\n", pItem->GetDisplayString());
		return;
	}
	// select it
	SetFocus(); // so selection will show (like replaying mouse-activate)
	GetTreeCtrl().SelectItem(hItem); // (NULL hItem OK, -> no selection)
}

BOOL CPrincView::DispatchEvent(EventID event, LPCTSTR pszArgs)
{
	CPrincItem* pItem;
	HTREEITEM hItem;
	char szArg1[128] = "";

	switch(event)
	{
	case EV_PRINCIPLE_NEW:
		OnPrincipleAdd();
		break;

	case EV_PRINCIPLE_SELECT:
		// get arg and find princitem (parent or child)
		pItem = ArgToItem(pszArgs);
		if (pItem == NULL && strcmp(pszArgs, szNullArg) != 0) return FALSE;
		SelectItem(pItem);
	break;

	case EV_PRINCIPLE_MODIFY:
		// verify selected principle from argument
		if ((pItem = ArgToItem(pszArgs)) == NULL) return FALSE;
		SelectItem(pItem);

		OnPrincipleModify();
		break;

	case EV_PRINCIPLE_DELETE:
		// verify  selected principle from argument
		if ((pItem = ArgToItem(pszArgs)) == NULL) return FALSE;
		SelectItem(pItem);

		OnPrincipleDelete();
		break;

	case EV_PRINCIPLE_WHATSWRONG:
		// verify  selected principle from argument
		if ((pItem = ArgToItem(pszArgs)) == NULL) return FALSE;
		SelectItem(pItem);

		OnHelpWhatswrong();
		break;

	case EV_PRINCIPLE_SETCHECK:
		// note must extract first arg
		if (sscanf(pszArgs, "%s", szArg1) != 1) return FALSE; 
		if ((pItem = ArgToItem(szArg1)) == NULL) return FALSE;
		// !!! note can toggle item without selecting it
		hItem = FindPrincItem(pItem);
		if (hItem)
			ToggleCheckState(hItem);
		break;

	case EV_PRINCIPLE_MENU:
		// ignore, just handle command selected
		break;

	default:
		return FALSE;
	}
	return TRUE;
}

void CPrincView::PointToObject(LPCTSTR pszObjID)
{
}








