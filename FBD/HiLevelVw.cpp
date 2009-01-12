// HiLevelVw.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "fbddoc.h"
#include "helpifc.h"
#include "PropertyDlg.h"
#include "VariableDlg.h"
#include "MainFrm.h"
#include "StageObj.h"
#include "HiLevelVw.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



#include "ChildFrm.h"
/////////////////////////////////////////////////////////////////////////////
// CHiLevelVw

IMPLEMENT_DYNCREATE(CHiLevelVw, CScrollView)

CHiLevelVw::CHiLevelVw()
{
	m_pFocusedTable = NULL;
	m_pFocusedItem = NULL;
//	m_pTimeItem = NULL;
}

CHiLevelVw::~CHiLevelVw()
{
//	if (m_pTimeItem)
//		delete m_pTimeItem;
	delete m_pFont;
	delete m_pBoldFont;
}


BEGIN_MESSAGE_MAP(CHiLevelVw, CScrollView)
	//{{AFX_MSG_MAP(CHiLevelVw)
	ON_WM_CREATE()
	ON_WM_CTLCOLOR()
	ON_WM_SIZE()
	ON_COMMAND(ID_HELP_WHATSWRONG, OnHelpWhatswrong)
	ON_UPDATE_COMMAND_UI(ID_HELP_WHATSWRONG, OnUpdateHelpWhatswrong)
	ON_WM_CONTEXTMENU()
	ON_UPDATE_COMMAND_UI(ID_ADDPROP_TIME, OnUpdateAddpropTime)
	//}}AFX_MSG_MAP
		// Events from run-time choice button controls:
	ON_COMMAND_RANGE(ID_DIRECTION_FIRST, ID_DIRECTION_LAST, OnDirectionMenuCmd)
	ON_UPDATE_COMMAND_UI_RANGE(ID_PARALLEL_FIRST, ID_PARALLEL_LAST, OnUpdateDirectionParallel)
	ON_COMMAND_RANGE(ID_ADDPROP_FIRST, ID_ADDPROP_LAST, OnAddPropMenuCmd)
	ON_CONTROL_RANGE(BN_CLICKED, IDG_TABLEGRID_FIRST, IDG_TABLEGRID_LAST, OnClickGrid)
    ON_MESSAGE(WM_GETFONT, OnGetFont)
	// Tooltip text notification callbacks
	ON_NOTIFY_EX( TTN_NEEDTEXT, 0, OnToolTipNotify)

END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHiLevelVw drawing

void CHiLevelVw::OnDraw(CDC* pDC)
{
	CDocument* pDoc = GetDocument();

}

/////////////////////////////////////////////////////////////////////////////
// CHiLevelVw diagnostics

#ifdef _DEBUG
void CHiLevelVw::AssertValid() const
{
	CView::AssertValid();
}

void CHiLevelVw::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CFBDDoc* CHiLevelVw::GetDocument() // non-debug version is inline
{
	ASSERT(((CChildFrame*)GetParentFrame())->m_pDoc->IsKindOf(RUNTIME_CLASS(CFBDDoc)));
	return ((CChildFrame*)GetParentFrame())->m_pDoc;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CHiLevelVw message handlers

void CHiLevelVw::OnInitialUpdate() 
{
	CScrollView::OnInitialUpdate();

	UpdateScrollSize();//init scrolling for view

	int nStage = GetDocument()->m_stages.GetCount();
	if (nStage > 0){
		POSITION stgPos = GetDocument()->m_stages.GetHeadPosition();
		while (stgPos != NULL)
		{
			CStageObj* pStage = GetDocument()->m_stages.GetNext(stgPos);
			pStage->m_pView = this;
			//insert the stage items
			for (int j=0; j< pStage->m_items.GetSize(); j++)
			{
				COutlineItem* pItem = pStage->m_items[j];
				pItem->m_pStage = pStage;
				POSITION dtaPos = pItem->m_datas.GetHeadPosition();

				while (dtaPos != NULL)
				{
					CCtrlData* pData = pItem->m_datas.GetNext(dtaPos);
					CItemCtrl* pCtrl;
					if (pData->m_chType != 't'){
						pCtrl = pItem->AddControl(this, pData->m_chType, pData->m_strLabel);
						pCtrl->m_bInitState = pData->m_bInitState;//needs to be here
						//so control sent to help system (!m_bInitState)
						if (pCtrl->IsKindOf(RUNTIME_CLASS(CCellCtrl))){
							CCellCtrl* pCell = (CCellCtrl*)pCtrl;
							if (!pData->m_properties.IsEmpty())
							{
								CCheckedObj* pProp = (CCheckedObj*)pData->m_properties.RemoveHead();
								pCell->m_properties.AddTail(pProp);
							}
						}
						// Notify help system about new object and its id.
						if ( pCtrl->IsKindOf(RUNTIME_CLASS(CCheckedItem)) )
							((CCheckedItem*) pCtrl)->CheckObject();
					}
					else{
						pCtrl = pItem->ReadInTable(pData);
					}
					if (pCtrl != NULL)//shouldn't happen
					{
						pCtrl->m_bInitState = pData->m_bInitState;
						pCtrl->Enable(!pStage->m_pPrinc->m_bChecked);
					}

				}
			}
			pStage->EnableBtn(FALSE, IDB_BTN_DELPROP);//nothing selected, delete btn disabled

		}


	}

	UpdateScrollSize();//Update size after template inserted
	EnableToolTips();

}
 

int CHiLevelVw::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CScrollView::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	m_pFont = new CFont();//we need to create and select the
 	m_pFont->CreatePointFont(80, "MS Sans Serif");//font that is used

	m_pBoldFont = new CFont();
    LOGFONT lf;

    //create bold font for statics
    VERIFY(m_pFont->GetLogFont(&lf));
    lf.lfWeight = FW_BOLD;
    VERIFY(m_pBoldFont->CreateFontIndirect(&lf));


	return 0;
}


void CHiLevelVw::OnDirectionMenuCmd(UINT nID)
{
	LogEventf(EV_EDIT_DIRECTION, "%d", nID);
	CStageObj* pStage = m_pFocusedItem->GetStage();

	if ( (nID == ID_VERTICAL) || (nID == ID_HORIZONTAL) || (nID == ID_VERTANDHORZ) )
	{
		CString	btnStr = "Show equation";
		CString	stcStr = "Apply law in direction:";
		pStage->m_nEqs = 1;

		CString str;
		if (nID == ID_VERTICAL)
			str = "vertical";
		else if (nID == ID_HORIZONTAL)
			str = "horizontal";
		else{
			str = "vertical and horizontal";
			stcStr = "Apply law in two directions:";
			pStage->m_nEqs = 2;
		}

		CItemCtrl* pStc = m_pFocusedItem->m_controls[0];
		if (pStc != NULL){
			pStc->SetText(stcStr);
			m_pFocusedItem->UpdateCtrlsthatFollow(pStc);
		}

		CCellCtrl* pCell =	m_pFocusedItem->ReplaceCellText(str);
		//if necessary, delete property attached to cell
		if (!pCell->m_properties.IsEmpty())
			delete pCell->m_properties.RemoveHead();

		if (pCell->m_bInitState)
		{
			pCell->m_bInitState = FALSE;
			m_pFocusedItem->AddControl(this, 'b', "Show equation");
		}
		else
			m_pFocusedItem->UpdateCtrlsthatFollow(pCell);
		pCell->CheckObject();

		//once he menu pops up, it takes over with its own message loop
		//so the cell never get the onMouseMove message that 
		//sets the BOOLEAN below = false
//		pCell->m_MouseOnBtn = FALSE;

		GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_OUTLINEITEM, m_pFocusedItem);
	}
	else 
	{
		CString txt = "Please specify direction(s) by selecting one of the vector quantities from the properties table";
		theApp.DoInstructMessage(txt);

		if (nID == ID_OTHER)
			pStage->m_nEqs = 1;
		else
			pStage->m_nEqs = 2;

		pStage->HighlightTables(TRUE, nID);
		pStage->EnableBtn(FALSE, IDB_BTN_DELPROP);
		pStage->EnableBtn(FALSE, IDB_BTN_ADDPROP);

	}
	GetDocument()->SetModifiedFlag();

}


void CHiLevelVw::OnUpdateDirectionParallel(CCmdUI* pCmdUI) 
{
	//"parallel to..." in the direction menu can only be enabled when either
	//a force table exists (i.e. there is a force property) OR a kinematics
	//table exists and has a property other than time. 
	//We can only choose directions parallel to vectors
	CStageObj* pStage = m_pFocusedItem->GetStage();
	POSITION pos = pStage->m_tables.GetHeadPosition();
	while (pos != NULL)
	{
		CItemCtrl* pCtrl = pStage->m_tables.GetNext(pos);
		if (pCtrl->GetCtrlID() == IDG_FORCEPROP){
			pCmdUI->Enable(TRUE);
			return;
		}
		else if (pCtrl->GetCtrlID() == IDG_KINEMATICPROP){
			CTableGrid* pTable = (CTableGrid*)pCtrl;
			POSITION pos = pTable->m_properties.GetHeadPosition();
			while (pos != NULL)
			{
				CTableRow* pProp = pTable->m_properties.GetNext(pos);
				if (pProp->m_strQuantName.Find("time") == -1)
				{
					pCmdUI->Enable(TRUE);
					return;
				}
			}
		}
	}
	pCmdUI->Enable(FALSE);

}



void CHiLevelVw::OnAddPropMenuCmd(UINT nID)
{
	LogEventf(EV_EDIT_PROPERTY, "%d", nID);

	CStageObj* pStage = m_pFocusedItem->GetStage();

	CTableRow* pProp = new CTableRow();
	GetDocument()->GenerateId(pProp);//generate unique ID
  									
	int nType = pProp->SetType(nID); //Sets property type, returns table type
	int id = nType + (pStage->m_nId * 100);//Ctrl id

	//if the table of that type already exists, find it
	CTableGrid* pTable = pStage->GetTable(id);
	//otherwise create the table
	BOOL m_bAddTable = FALSE;

	if (pTable == NULL)
	{
		COutlineItem* pItem = pStage->InsertTableItem();
		pTable = pItem->AddTable(id);

		m_bAddTable = TRUE;
	}

	pProp->m_pTable = pTable;

	if (!pProp->OnEditProperties())
	{
		pProp->NotifyDelete();
		pProp->Delete();
		if (m_bAddTable)
			pStage->DeleteTable(pTable);
		return;
	}

	if (m_bAddTable)
	{
		pTable->Initialize();
		pStage->AddTable(pTable);
	}
	
	//add the property to the table
	pTable->AddProperty(pProp);

	COutlineItem* pItem = pTable->GetOutlineItem();
	pStage->UpdateItemsthatFollow(pItem);
	UpdateStagesthatFollow(pStage);
	GetDocument()->SetModifiedFlag();
	GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_HILEVELVW, NULL);



}

void CHiLevelVw::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
{
	// In CScrollView,  this will adjust viewport origin to match 
	// current scrolled position in the document:
	CScrollView::OnPrepareDC(pDC, pInfo);

	// Now set up to map logical units to this device:
	pDC->SetMapMode(MM_ANISOTROPIC);
	pDC->SetViewportExt(pDC->GetDeviceCaps(LOGPIXELSX),
						pDC->GetDeviceCaps(LOGPIXELSY));
	pDC->SetWindowExt(nLUsPerInch, nLUsPerInch);

	// if custom default font set, select it into DC before all drawing.
	if (m_pFont)
		pDC->SelectObject(m_pFont);
	
	CView::OnPrepareDC(pDC, pInfo);
}

BOOL CHiLevelVw::OnScrollBy(CSize sizeScroll, BOOL bDoScroll) 
{

	// Do the scroll
	if (! CScrollView::OnScrollBy(sizeScroll, bDoScroll))
		return FALSE;
	// update the position of any in-place active item
	if (bDoScroll) {
		UpdateWindow();
	}
	return TRUE;

}	


void CHiLevelVw::OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint) 
{
	// ignore updates that do not affect plan objects
	if (! (IS_PLAN_UPDATE(lHint) ) )
		return;

	CSize size;
	CStageObj* pStage;

	switch (lHint)
	{
	default:

	case HINT_UPDATE_HILEVELVW:	// redraw entire diagram window
		UpdateScrollSize();
		Invalidate(FALSE);
		break;


	case HINT_UPDATE_OUTLINEITEM:	// a single Outline item has changed
		UpdateScrollSize();
		InvalItemInView((COutlineItem*) pHint);
		break;

	case HINT_DELETE_PROP:
	case HINT_ADD_PROP:
		{
			CStageObj* pStage = (CStageObj*) pHint;
			UpdateStagesthatFollow(pStage);
		}break;
	
	case HINT_DELETE_PRINC:
		{
			CPrincObj* pPrinc = (CPrincObj*) pHint;
			POSITION pos = GetDocument()->m_stages.Find(pPrinc->m_pStageObj);//find position
			pStage = GetDocument()->m_stages.GetNext(pos);//get this stage (the stage we are deleting)
			if (pos != NULL)
			{
				CStageObj* pNextStage = GetDocument()->m_stages.GetNext(pos);
				int yDist = pStage->GetPosition().top  - pNextStage->GetPosition().top;
				pNextStage->MoveStage(0, yDist);
				UpdateStagesthatFollow(pNextStage);
			}
		} break;
	case HINT_ADD_PRINC:
		{
			CPrincObj* pPrinc = (CPrincObj*) pHint;
			pPrinc->m_pStageObj->m_pView = this;
			COutlineItem* pLast = NULL;
			//GetLast stage
			CStageObj* pPrevStage = pPrinc->m_pStageObj;
			//should have been added when we added the principle to the document
			ASSERT(!GetDocument()->m_stages.IsEmpty());
			POSITION pos = GetDocument()->m_stages.GetTailPosition();//most recently added (tail)
			while ( (pos != NULL) && (pPrevStage == pPrinc->m_pStageObj))
				pPrevStage = GetDocument()->m_stages.GetPrev(pos);//want stage before the one we just added

			if (pPrevStage && (pPrevStage != pPrinc->m_pStageObj)  )
				pLast = pPrevStage->GetLast();//Get last outline item of previous stage

			for (int j=0; j< pPrinc->m_pStageObj->m_items.GetSize(); j++)
			{
				COutlineItem* pItem = pPrinc->m_pStageObj->m_items[j];
				pItem->UpdatePosition(pLast);
				POSITION pos =  pItem->m_datas.GetHeadPosition();
				while (pos != NULL)
				{
					CCtrlData* pData = pItem->m_datas.GetNext(pos);
					pItem->AddControl(this, pData->m_chType, pData->m_strLabel);
				}
				pLast = pItem;
			}

			pPrinc->m_pStageObj->EnableBtn(FALSE, IDB_BTN_DELPROP);//nothing selected, delete btn disabled


		}break;

	case HINT_CHKCHANGE_PRINC:
		{
			CPrincObj* pPrinc = (CPrincObj*) pHint;
			CStageObj* pStage = pPrinc->m_pStageObj;
			pStage->Enable(!pPrinc->m_bChecked);
			pStage->EnableBtn(FALSE, IDB_BTN_DELPROP);//nothing selected, delete btn disabled
		}break;
	
	case HINT_UPDATE_PRINC:
		{
			ASSERT_KINDOF(CPrincObj, pHint);
			CPrincObj* pPrinc = (CPrincObj*) pHint;
			CStageObj* pStage = pPrinc->m_pStageObj;
			// only need to update law item for this stage.
			COutlineItem* pLawItem = pStage->GetItem(ITEM_LAW);
			if (!pLawItem) return;
			// find static control for law, should be first and only one
			CItemCtrl* pCtrl = pLawItem->m_controls[0]; 
			if (! pCtrl) return;
			// then set its text to new 
			CString strNew = pPrinc->GetDisplayString();
			pCtrl->SetText(strNew);
		
		} break;
		
	}
	
}


HBRUSH CHiLevelVw::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	HBRUSH hbr = CScrollView::OnCtlColor(pDC, pWnd, nCtlColor);
	
	// TODO: Change any attributes of the DC here
	if (nCtlColor == CTLCOLOR_STATIC){
		pDC->SetBkColor(RGB(255,255,255));
		return ::GetSysColorBrush(COLOR_WINDOW);

	}
	// TODO: Return a different brush if the default is not desired
	return hbr;
}

void CHiLevelVw::InvalItemInView(COutlineItem* pItem) 
{
	CRect dirty = pItem->GetPosition();
	dirty.InflateRect(1, 1); 
	InvalidateRect(dirty, TRUE);
//	UpdateWindow();
}

void CHiLevelVw::OnSize(UINT nType, int cx, int cy) 
{
	CScrollView::OnSize(nType, cx, cy);
	
	UpdateWindow();//Do i need this here?
	
}

void CHiLevelVw::UpdateScrollSize()
{
	//Adapted from DRAWCLI code to set scroll sizes using MM_TEXT mapping mode
	CClientDC dc(this);
	CSize size = GetDocument()->GetHiLevelPlanSize(); 
	// For now, set tiny size to prevent scroll bars from showing
	//CSize size(1 * nLUsPerInch, 8 * nLUsPerInch);
	size.cx = MulDiv(size.cx, dc.GetDeviceCaps(LOGPIXELSX), nLUsPerInch) + 100;
	size.cy = MulDiv(size.cy, dc.GetDeviceCaps(LOGPIXELSY), nLUsPerInch) + 100;
	SetScrollSizes(MM_TEXT, size);
}



void CHiLevelVw::OnClickGrid(UINT gridId) 
{ 
	CStageObj* pStage = m_pFocusedItem->GetStage();
	POSITION pos = pStage->m_tables.GetHeadPosition();
	BOOL bTableIsEnabled = FALSE;
	while (pos != NULL)
	{
		CTableGrid* pTable = (CTableGrid*)pStage->m_tables.GetNext(pos);
		if (pTable->GetId() == (int)gridId)
			bTableIsEnabled = pTable->IsWindowEnabled();
		else{
			pTable->Deselect();
			if (pTable->IsHighlighted()){
				pTable->Highlight(FALSE);
				pTable->Invalidate();
			}
		}
	}
	
}

void CHiLevelVw::UpdateFromUserSelection(CTableRow* pProp)
{
	CStageObj* pStage = m_pFocusedItem->GetStage();

	CString btnStr;//button string (show equation(s))
	CString stcStr;//static string (apply law in (two) direction(s))
	CString str = pProp->m_strDef;//definition of chosen parallel property

	if (pStage->m_nEqs == 2){
		str = "parallel and perpendicular to the direction of " + str;
		btnStr = "Show equations";
		stcStr = "Apply law in two directions:";
	}
	else{
		str = "parallel to the direction of " + str;
		btnStr = "Show equation";
		stcStr = "Apply law in direction:";
	}
	//fill in direction cell
	COutlineItem* pItem = pStage->GetItem(ITEM_DIRECTION);

	CCellCtrl* pCell = pItem->ReplaceCellText(str);
	//if first time clicked, add show equations button
	if (pCell->m_bInitState)
	{
		pCell->m_bInitState = FALSE;
		pItem->AddControl(this, 'b', btnStr);
	}
	//Remove existing property attached to cell, if any
	if (!pCell->m_properties.IsEmpty())
		delete pCell->m_properties.RemoveHead();
	//attach new property to celll
	CTableRow* pClone = (CTableRow*)pProp->Clone();
	pCell->m_properties.AddTail(pClone);
	//Set static text to reflect 1 or more equations
	CItemCtrl* pStc = pItem->m_controls[0];
	if (pStc != NULL){
		pStc->SetText(stcStr);
		pItem->UpdateCtrlsthatFollow(pStc);
	}
		
	pItem->UpdateCtrlsthatFollow(pCell);
	//once he menu pops up, it takes over with its own message loop
	//so the cell never get the onMouseMove message that 
	//sets the BOOLEAN below = false
//	pCell->m_MouseOnBtn = FALSE;
	GetDocument()->UpdateAllViews(NULL, HINT_UPDATE_OUTLINEITEM, pItem);
	pCell->CheckObject();

	
}

void CHiLevelVw::UpdateStagesthatFollow(CStageObj* pStage)
{

	POSITION pos = GetDocument()->m_stages.Find(pStage);
	CStageObj* pThisStage;
	if (pos != NULL)
		pThisStage = GetDocument()->m_stages.GetNext(pos);
	while (pos != NULL){
		CStageObj* pNextStage = GetDocument()->m_stages.GetNext(pos);
		int yDist = pThisStage->GetPosition().bottom + VERTICAL_SPACING  + STAGE_SPACING - pNextStage->GetPosition().top;
		pNextStage->MoveStage(0, yDist);
		pThisStage = pNextStage;
	}

}


LRESULT CHiLevelVw::OnGetFont(WPARAM /*wParam*/, LPARAM /*lParam*/)
{
    return (LRESULT) m_pFont->m_hObject;
}


BOOL CHiLevelVw::DispatchEvent(EventID nEvent, LPCTSTR parms)
{
		
	int idFrom;
	int xpos, ypos;
	int nSel;
	switch(nEvent)
	{
	case EV_PLANCELL_CLICK:
	case EV_PLANBTN_CLICK:	
		{
		if (sscanf(parms, "%d", &idFrom) == 0)
			return FALSE;
		CWnd* pWnd = GetDlgItem(idFrom);
		if (pWnd == NULL)
			return FALSE;

		if (nEvent == EV_PLANCELL_CLICK)
			pWnd->SendMessage(WM_COMMAND, MAKELONG(idFrom, BN_CLICKED), LPARAM (pWnd->m_hWnd ));
		else//EV_PLANBTN_CLICK
			pWnd->SendMessage(BM_CLICK);

		}break;

	case EV_EDIT_SOUGHT:
	case EV_EDIT_PROPERTY:
	case EV_EDIT_DIRECTION:
	case EV_EDIT_NEXTSTEP:
		{
		if (sscanf(parms, "%d", &idFrom) == 0)
			return FALSE;
	/*	if (nEvent == EV_EDIT_SOUGHT) OnSoughtMenuCmd(idFrom);
		else*/ if (nEvent == EV_EDIT_PROPERTY) OnAddPropMenuCmd(idFrom);
		else if (nEvent == EV_EDIT_DIRECTION) OnDirectionMenuCmd(idFrom);
	//	else if (nEvent == EV_EDIT_NEXTSTEP); //OnNextMenuCmd(idFrom);
		}
		break;

	case EV_LBUTTONDOWN_GRID:
	case EV_MOUSEUP_GRID:		
	case EV_LBUTTONDBLCLK_GRID:
	case EV_MOUSEMOVE_LIST:
		{
		if (sscanf(parms, "%d %d %d", &idFrom, &xpos, &ypos) == 0)
			return FALSE;

		CWnd* pWnd = GetDlgItem(idFrom);
		if (pWnd == NULL)
			return FALSE;

		if (nEvent == EV_LBUTTONDOWN_GRID)
			pWnd->SendMessage(WM_LBUTTONDOWN, WPARAM (0 ), MAKELONG(xpos, ypos));
		else if (nEvent == EV_MOUSEUP_GRID)
			pWnd->SendMessage(WM_LBUTTONUP, WPARAM (0 ), MAKELONG(xpos, ypos));
		else if (nEvent == EV_LBUTTONDBLCLK_GRID)
			pWnd->SendMessage(WM_LBUTTONDBLCLK, WPARAM (0 ), MAKELONG(xpos, ypos));
		else if (nEvent == EV_MOUSEMOVE_LIST)
			pWnd->SendMessage(WM_MOUSEMOVE, WPARAM(0), MAKELONG(xpos, ypos));
		}
		break;

	case EV_PLANLIST_SEL:{
		if (sscanf(parms, "%d %d", &idFrom, &nSel) == 0)
			return FALSE;

		CWnd* pWnd = GetDlgItem(idFrom);
		if (pWnd == NULL)
			return FALSE;

		((CListBox*)pWnd)->SetCurSel(nSel);
		pWnd->SendMessage(WM_COMMAND, MAKELONG(idFrom, LBN_SELCHANGE), LPARAM(pWnd->m_hWnd));

		}
		break;
	
	default:
		break;
	}
	return TRUE;
}


void CHiLevelVw::OnHelpWhatswrong() 
{
	LPCTSTR	pszResult;
	CString stageId;
	CString strHelp;
	int nStageId = m_pFocusedItem->GetStage()->m_nId;
	stageId.Format("stage-%d", nStageId);
	if (m_pFocusedItem->GetId() == ITEM_TABLE)
	{
		CGridCtrl* pGrid = m_pFocusedItem->GetGrid();
		int id = pGrid->GetId() - (nStageId*100);

		int nRow = pGrid->GetFocusCell().row;
		if (nRow <= 0)
			return;
		int nCol = pGrid->GetFocusCell().col;
		CGridCell* pCell = pGrid->GetCell(nRow, nCol);
		CTableRow* pProp = (CTableRow*)pCell->lParam;
		if (nCol == 0)
			pszResult = HelpSystemExecf("(Why-wrong-object %s) ", 
									STR2ARG(pProp->m_strId) );
		else{
			if (!pCell->szMagDir.IsEmpty()){
				strHelp.Format("%s-%s", pCell->szMagDir, pProp->m_strId);
			}
			else
				strHelp = pProp->m_strId;
			pszResult =
				HelpSystemExecf( "(why-wrong-plan-item %s known-status %s)",
					STR2ARG(stageId),
					STR2ARG(strHelp)
				);
		}



	}
	else
	{
		if (m_pFocusedItem->GetId() == ITEM_DIRECTION)
			strHelp = "direction";
		if (m_pFocusedItem->GetId() == ITEM_CHKSOLV){
			for (int i=0; i<m_pFocusedItem->m_controls.GetSize(); i++){
				CItemCtrl* pCtrl = m_pFocusedItem->m_controls[i];
				if (pCtrl->IsKindOf(RUNTIME_CLASS(CMenuGrid))){
					CMenuGrid* pGrid = (CMenuGrid*)pCtrl;
					if (pGrid->IsFocused()){
						strHelp = pGrid->GetHelpString();
					}
				}
			}

		}
		pszResult =
			HelpSystemExecf( "(why-wrong-plan-item %s %s)",
				STR2ARG(stageId),
				STR2ARG(strHelp)
				);
	}


	theApp.GetMainFrame()->ShowHint(pszResult, WhatsWrong);
}

void CHiLevelVw::OnUpdateHelpWhatswrong(CCmdUI* pCmdUI) 
{
	if (m_pFocusedItem == NULL)
	{
		pCmdUI->Enable(FALSE);
		return;
	}
	if (m_pFocusedItem->GetId() == ITEM_TABLE)
	{
		CGridCtrl* pGrid = m_pFocusedItem->GetGrid();
		CCellID cell = pGrid->GetFocusCell();
		CGridCell* pCurCell = pGrid->GetCell(cell.row, cell.col);

		pCmdUI->Enable(m_pFocusedItem && pCurCell && pCurCell->m_status == statusError);

	}
	else if((m_pFocusedItem->GetId() == ITEM_CHKSOLV) )
	{
		CMenuGrid* pGrid = m_pFocusedItem->GetFocusedGrid();
		pCmdUI->Enable(m_pFocusedItem && pGrid && pGrid->GetStatus() == statusError);
	}
	else 
	{
	
		if ((m_pFocusedItem->m_nItemType > ITEMTYPE_LAST) || 
			(m_pFocusedItem->m_nItemType > ITEMTYPE_LAST) )
			int T = 5;
		CCellCtrl* pCtrl = m_pFocusedItem->GetCell();
		pCmdUI->Enable(m_pFocusedItem && pCtrl && pCtrl->GetStatus() == statusError);
	}



}

void CHiLevelVw::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	CPoint local = point;	// point for this message comes in screen coords
	ScreenToClient(&local);
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.DPtoLP(&local);


	// Make sure click selected an object, ignoring Unselectable problem objects
	// (Would also like to "activate" (set focus) an Answer Box, which is unselectable since
	// doc object is Problem obj. Now handled in control by grabbing focus on RClick. 
	COutlineItem* pItem;
	if ((pItem = GetDocument()->ItemAt(local)) != NULL)
	{
		if (m_pFocusedItem != pItem){//if we are not clicking on the selected object or selected part
			pItem->SetFocus();//try to select the one we are over//focus set
		}

		if (pItem->m_nItemType == ITEM_CHKSOLV){
			CItemCtrl* pCtrl = pItem->CtrlAt(local);
			for (int i=0; i<pItem->m_controls.GetSize(); i++){
				CItemCtrl* pCtrlInList = pItem->m_controls[i];
				pCtrlInList->SetFocus(pCtrl==pCtrlInList);//unset all but 1
			}
		}

		// didn't log right-click selection change here -- will be replayed by FBD_MENU event

		LogEventf(EV_HILEVELVW_MENU, "%d %d  %d %d", point.x, point.y, local.x, local.y);

		CMenu menu;
		VERIFY(menu.LoadMenu(IDR_POPUP_HILEVELVW));
		CMenu* pPopup = menu.GetSubMenu(0);
		ASSERT(pPopup != NULL);


		CWnd* pWndPopupOwner = this;
		while (pWndPopupOwner->GetStyle() & WS_CHILD)
			pWndPopupOwner = pWndPopupOwner->GetParent();

		pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
			pWndPopupOwner);
	}
/*	else // we are not clicking on an object
	{
		Select(NULL);//we deselect any selected object
		return;	
	}	*/
}
// Tooltip support:
//
// With tooltips enabled, CWnds filter every mouse message, relaying data to MFC's 
// shared tooltip control. The filter calls following virtual function to obtain info
// about the currently hit "tool" (child window or rectangular region). 
// We override to provide data for tooltips that treat FBD objects as "tools".
// Cf. sample at http://ourworld.compuserve.com/homepages/MRConway/tooltip.htm
//
COutlineItem* CHiLevelVw::s_pTipObj = NULL;			// static saves obj on hits for text callback

int CHiLevelVw::OnToolHitTest( CPoint point, TOOLINFO* pTI ) const
{
	// MFC declared function const, making type of "this" ptr const within it. Have to
	// cast away constness of "this", since didn't declare all methods used as const.
	CHiLevelVw* pThis = (CHiLevelVw*) this;	
	
	// Find object at point, saving into static s_pTipObj. 
	CPoint local = point;					// local = point in logical coords
	pThis->ClientToDoc(local);
	s_pTipObj = pThis->GetDocument()->ItemAt(local);
	if (s_pTipObj == NULL)		// no hit
		return -1;
	
	// We must fill in TOOLINFO to return data about hit "tool" region.
	pTI->hwnd = m_hWnd;						// handle of containing wnd
	CRect rcObj = s_pTipObj->GetPosition();
	pThis->DocToClient(rcObj);
	pTI->rect = rcObj;						// tool bounding rect, client coords
	//
	// Set id sent with TTN_NEEDTEXT callback, normally a cmd id or child ctl hwnd. 
	// We use a distinguished id, so can tell it's not a standard cmd in OnToolTipNotify,
	// see below. Note id must be unique among all views using custom tooltips.
	//
#	define HILEVELVW_TOOLID ((UINT) -2)		// id for FBDVIEW drawobj tool callbacks
	pTI->uId = HILEVELVW_TOOLID; 
	pTI->uFlags &= ~TTF_IDISHWND;			// id is not an hwnd, in case it matters
	pTI->uFlags |= TTF_ALWAYSTIP;			// show tips even when inactive (from sample)
	pTI->lpszText = LPSTR_TEXTCALLBACK;		// retrieve text via TTN_NEEDTEXT callback 

	// must return cookie MFC can use to determine when hit "tool" changes
	return int(s_pTipObj);					// just use object pointer value
}

// Handle TTN_NEEDTEXT notification callback from tooltip ctl to supply text for tip
// Because notifications are routed like commands, view may receive this msg
// for all tools in app, including, e.g., tools in mainframe toolbar. For that reason
// handler is mapped by a NOTIFY_EX entry, which passes msg up if not consumed.
// 
BOOL CHiLevelVw::OnToolTipNotify( UINT id, NMHDR * pNMHDR, LRESULT * pResult )
{
	TOOLTIPTEXT *pTTT = (TOOLTIPTEXT *)pNMHDR;
	// Ensure this is a request for a CFBDView "tool" by checking that ID in request 
	// is the same as the distinguished one we supplied in OnToolHitTest
    if (pNMHDR->idFrom == HILEVELVW_TOOLID)
	{
		// get hit obj from saved static
		if (!s_pTipObj)	return FALSE;	// shouldn't happen
				
		// use print def to show object properties in tooltip
		// in author mode, append object id to tip
		if (theApp.m_bAuthorMode)
			sprintf(pTTT->szText, "%s ID=%s", s_pTipObj->GetTipDef(), 
											 /*s_pTipObj->m_strId*/ "Plan");
		else
			strcpy(pTTT->szText, s_pTipObj->GetTipDef());	
		
		return(TRUE);
	}
	
	return FALSE;
}
// 
// Coordinate conversions for scrolling view:
//
void CHiLevelVw::ClientToDoc(CPoint& point)
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.DPtoLP(&point);
}

void CHiLevelVw::ClientToDoc(CRect& rect)	// verifies result is normalized
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.DPtoLP(&rect);

	// ASSERTS adapted from DRAWCLI to use our y orientation.
	// Not clear if this is appropriate requirement for us, might use to
	// convert non-normalized client rects.
	// (Result will be normalized just in case input client rect is.)
	ASSERT(rect.left <= rect.right);
	ASSERT(rect.top <= rect.bottom);
}

void CHiLevelVw::DocToClient(CPoint& point)
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.LPtoDP(&point);
}

void CHiLevelVw::DocToClient(CRect& rect)	// Note: returns normalized rect
{
	CClientDC dc(this);
	OnPrepareDC(&dc, NULL);
	dc.LPtoDP(&rect);
	// Following from DRAWCLI, not clear if appropriate for us.
	// Guarantees result is normalized client rect, useable in GDI computations.
	// Loses orientation of doc rect if it was needed.
	rect.NormalizeRect();
}

void CHiLevelVw::OnUpdateAddpropTime(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(!GetDocument()->m_strTimes.IsEmpty());
	
}
