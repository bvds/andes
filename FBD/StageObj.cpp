// CStageObj.cpp : implementation file
// stage objects - exist in the high level solution window

#include "stdafx.h"
#include "fbd.h"
#include "fbddoc.h"
#include "helpifc.h"
#include "HiLevelVw.h"
#include "StageObj.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif



#define ID_CELLBASE			12000
#define ID_STCBASE			13000
#define ID_BTNBASE			14000
#define ID_GRIDBASE			15000



extern const OUTLINEFORMAT items[] = 
{
	ITEM_LAW,		's', "Applying Law to body",
	ITEM_SYSPROP,	's', "System properties:", 
	ITEM_SYSPROP,	'b', "Add",					
	ITEM_SYSPROP,	'b', "Delete",		
	ITEM_DIRECTION,	's', "Apply law in direction:", 
	ITEM_DIRECTION,	'c', "Choose direction",
	ITEM_CHKSOLV,	's', "We get",
	ITEM_CHKSOLV,	'g', "Count",
	ITEM_CHKSOLV,	's', "equations with",
	ITEM_CHKSOLV,	'g', "Count",
	ITEM_CHKSOLV,	's', " unknowns (including soughts)",

};

const int nItems ARRAY_SIZE(items);
/////////////////////////////////////////////////////////////////////////////
// CStageObj
//////////////////////////////////////////////////////////////////////////
// 
// Represents a step in the hi level solution plan
//
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CStageObj, CObject, VERSIONABLE_SCHEMA | 5);

CStageObj::CStageObj()
{
	m_nEqs = 0;
}

CStageObj::~CStageObj()
{
	for (int i=0; i<m_items.GetSize(); i++)
	{
		delete m_items[i];
	}
	
}

 
void CStageObj::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;

	CObject::Serialize(ar);
  	if (ar.IsStoring())
   	{//started with version 1
//		ar << m_strBody;
		ar << m_nId;
		ar << m_nEqs;
		ar << m_pPrinc;
		m_items.Serialize(ar);
	} else {
		m_pDocument = (CFBDDoc*) ar.m_pDocument;
    	ASSERT_VALID(m_pDocument);
    	ASSERT_KINDOF(CFBDDoc, m_pDocument);
		
		CString tempstr;
		if ((nClassVersion > 3) && (nClassVersion < 5))
			ar >> tempstr;
		
		if (nClassVersion > 1)
			ar >> m_nId;
		else 
			m_nId = ((CFBDDoc*)ar.m_pDocument)->nNextStageID++;

		if (nClassVersion > 2)
			ar >> m_nEqs;
	
		if (nClassVersion > 4)
			ar >> m_pPrinc;

		ASSERT(nDocVersion >= 11);
		ASSERT(nClassVersion >= 1);//started with version1
		m_items.Serialize(ar);// load stage items
	}

}

/////////////////////////////////////////////////////////////////////////////
// CStageObj message handlers

////////////////////////////////////////////////////////////////////////////

//Insert the blank stage template
void CStageObj::AddTemplate()
{
	for (int i=ITEMTYPE_FIRST; i<=ITEMTYPE_LAST; i++)
	{
		COutlineItem* pItem = new COutlineItem(i);
		pItem->m_pStage = this;
		m_items.Add(pItem);//add to stage's list of items
		pItem->AddDatas(i);

	}

}

void CStageObj::OnPropertyDelete()
{
	CTableGrid* pTable = m_pView->m_pFocusedTable;
	if (pTable != NULL){
		COutlineItem* pItem = pTable->GetOutlineItem();
		pTable->DeleteProperty();
		if (pTable->GetRowCount() == 1)
		{//Remove from table list
			//new pItem is the item with the focus, (sys props buttons)
			DeleteTable(pTable);
			pItem = m_pView->m_pFocusedItem;
		}

		UpdateItemsthatFollow(pItem);
		EnableBtn(FALSE, IDB_BTN_DELPROP);//Nothing seleted right after a delete
	}
	theApp.GetDocument()->SetModifiedFlag();

}

void CStageObj::DeleteTable(CTableGrid* pTable)
{
	POSITION pos = m_tables.Find(pTable);

	if (pos != NULL)
		m_tables.RemoveAt(pos);

	Delete(pTable->GetOutlineItem());//Delete Item (deletes control)

	m_pView->m_pFocusedTable = NULL;

}

void CStageObj::AddTable(CTableGrid* pTable)
{
	POSITION pos = m_tables.AddTail(pTable);
}


CTableGrid* CStageObj::GetTable(int nId)
{
	POSITION pos = m_tables.GetHeadPosition();
	while (pos != NULL)
	{
		CItemCtrl* pCtrl = m_tables.GetNext(pos);
		if (pCtrl->GetId() == nId){
			CTableGrid* pTable = (CTableGrid*)pCtrl;
			return pTable;
		}
	}
	return NULL;

}

void CStageObj::UpdateItemsthatFollow(COutlineItem* pItem)
{
	for (int i=0; i<m_items.GetSize(); i++)
	{
		COutlineItem* pThisItem = m_items[i];
		if (pThisItem == pItem)
		{
			for (int j=i+1; j<m_items.GetSize(); j++){
				COutlineItem* pNextItem = m_items[j];
				int yDist = pItem->GetPosition().bottom + VERTICAL_SPACING  - pNextItem->GetPosition().top;
				pNextItem->MoveItem(0, yDist);
				pItem = pNextItem;
			}
			break;
		}
	}

}
/*
void CStageObj::CollapseTables()
{
	POSITION pos = m_tables.GetHeadPosition();
	COutlineItem* pItem = m_pView->m_pFocusedItem;
	BOOL bIsCollapsed;
	while (pos != NULL)
	{
		CTableGrid* pTable = (CTableGrid*)m_tables.GetNext(pos);
		pTable->ToggleCollapse();//handles enabled state and selection
		bIsCollapsed = pTable->m_bCollapsed;
		pItem = pTable->GetOutlineItem();
	}
	EnableBtn(FALSE, IDB_BTN_DELPROP);//when collapsing, table disabled, when expanding, nothing selected
	EnableBtn(!bIsCollapsed, IDB_BTN_ADDPROP);

	COutlineItem* pNextItem = GetItem(ITEM_DIRECTION);
	if (pNextItem != NULL)
		pNextItem->Enable(bIsCollapsed);

}
*/
void CStageObj::HighlightTables(BOOL bHighlight, int nID)
{
	POSITION pos = m_tables.GetHeadPosition();
	COutlineItem* pItem = m_pView->m_pFocusedItem;
	while (pos != NULL)
	{
		CTableGrid* pTable = (CTableGrid*)m_tables.GetNext(pos);
		pTable->Highlight(bHighlight);
		pItem = pTable->GetOutlineItem();
	}

}




COutlineItem* CStageObj::GetNext(int nID)
{
	for (int i=0; i<m_items.GetSize(); i++)
	{
		COutlineItem* pItem = m_items[i];
		if (pItem->m_nItemType == nID){
			if ((i+1) < m_items.GetSize())
				return m_items[i+1];
			break;
		}
	}
	return NULL	;
}

COutlineItem* CStageObj::GetItem(int nID)
{
	for (int i=0; i<m_items.GetSize(); i++)
	{
		COutlineItem* pItem = m_items[i];
		if (pItem->GetId() == nID){
			return pItem;
		}
	}
	return NULL;
}

COutlineItem* CStageObj::GetLast()
{
	int nCount = m_items.GetSize();
	if (nCount > 0)
		return m_items[nCount - 1];
	
	return NULL;

}


CRect CStageObj::GetPosition()
{
	CRect rect(0, 0, 0, 0);
	for (int i=0; i<m_items.GetSize(); i++)
	{
		COutlineItem* pItem = m_items[i];
		CRect pos = pItem->GetPosition();
		if (i == 0){
			rect.top = pos.top;
			rect.left = pos.left;
		}

		if (rect.right < pos.right)
			rect.right = pos.right;
		if (rect.bottom < pos.bottom)
			rect.bottom = pos.bottom;

	}
	return rect;
}

void CStageObj::Delete(COutlineItem* pItem)
{
	if (pItem == m_pView->m_pFocusedItem)
		m_pView->m_pFocusedItem = NULL;


	for (int i=0; i<m_items.GetSize(); i++)
	{
		COutlineItem* pItemInList = m_items[i];
		if (pItemInList == pItem)
		{
			m_items.RemoveAt(i);//remove from list
		}
	}
	delete pItem;//delete item (the destructor deletes the items controls)

}

void CStageObj::EnableBtn(BOOL bEnable, UINT nBtnID)
{
	int nID = nBtnID + (100*m_nId);
	CWnd* pBtn = m_pView->GetDlgItem(nID);
	if (pBtn == NULL)
		return;
	((CItemCtrl*)pBtn)->Enable(bEnable);
}

void CStageObj::Enable(BOOL bEnable)
{
	for (int i =0; i<m_items.GetSize(); i++)
	{
		COutlineItem* pItem = m_items[i];
		pItem->Enable(bEnable);
	}
}

void CStageObj::MoveStage(int x, int y)
{
	for (int i=0; i<m_items.GetSize(); i++){
		COutlineItem* pItem = m_items[i];
		pItem->MoveItem(x, y);
	}
}


/////////////////////////////////////////////////////////////////////////////
// COutlineItem
//////////////////////////////////////////////////////////////////////////
// 
// Represents a item in the outline of a stage 
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(COutlineItem, CObject, VERSIONABLE_SCHEMA | 1);

COutlineItem::COutlineItem()
{
	SetPosition(CRect(LEFT_MARGIN, VERTICAL_SPACING, 
		LEFT_MARGIN+ITEM_WIDTH, VERTICAL_SPACING+ITEM_HEIGHT));

}

COutlineItem::COutlineItem(int nType)
{
	m_nItemType = nType;
	SetPosition(CRect(LEFT_MARGIN, VERTICAL_SPACING, 
		LEFT_MARGIN+ITEM_WIDTH, VERTICAL_SPACING+ITEM_HEIGHT));
//	SetPosition(CRect(HORIZONTAL_SPACING, VERTICAL_SPACING, 
//		HORIZONTAL_SPACING, VERTICAL_SPACING + ITEM_HEIGHT));
}

COutlineItem::~COutlineItem()
{
	for (int i=0; i < m_controls.GetSize(); i++)
	{
		 delete m_controls[i];
	}
	// Empty data list
	while (!m_datas.IsEmpty()){
		CCtrlData* pData = m_datas.RemoveHead();
		delete pData;
	}

}
   
void COutlineItem::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;

	CObject::Serialize(ar);
	WORD wTemp;
  	if (ar.IsStoring())
   	{//started with version 1
		 ar << m_position;
		 ar << (WORD)m_nItemType;
		 UpdateDatas();
	} else {
		ar >> m_position;
		ar >> wTemp;
		m_nItemType = (int) wTemp;
	}
	m_datas.Serialize(ar);	

}

/////////////////////////////////////////////////////////////////////////////
// COutlineItem message handlers

////////////////////////////////////////////////////////////////////////////

void COutlineItem::SetId(int id)
{
//	m_nId = id;
}

void COutlineItem::SetPosition(CRect pos)
{
	m_position = CRect(pos);
}


void COutlineItem::SetFocus()
{
	COutlineItem* pOldFocus = GetStage()->m_pView->m_pFocusedItem;
	
	if (pOldFocus && pOldFocus != this){
		for (int i=0; i < pOldFocus->m_controls.GetSize(); i++)
		{
			CItemCtrl* pCtrl = pOldFocus->m_controls[i];
			pCtrl->SetFocus(FALSE);
		}
	}


	GetStage()->m_pView->m_pFocusedItem = this;

	for (int j=0; j < m_controls.GetSize(); j++)
	{
		CItemCtrl* pCtrl = m_controls[j];
		pCtrl->SetFocus(TRUE);
	}


}

COutlineItem* CHiLevelVw::GetLastItem()
{
	if (GetDocument()->m_stages.GetCount() > 0)
	{
		CStageObj* pStage = GetDocument()->m_stages.GetTail();
		int j = pStage->m_items.GetSize();
		if (j>0)
			return pStage->m_items[j-1];
	}
	return NULL;
}

void COutlineItem::Enable(BOOL bEnable)
{
	for (int i=0; i<m_controls.GetSize(); i++){
		CItemCtrl* pCtrl = m_controls[i];
		pCtrl->Enable(bEnable);
	}

}

void COutlineItem::MoveItem(int x, int y)
{

	m_position.left += x;
	m_position.right += x;
	m_position.top += y;
	m_position.bottom += y;
	for (int i=0; i<m_controls.GetSize(); i++)
		m_controls[i]->UpdatePosition(x, y);
}

void COutlineItem::AddDatas(int nItemType)
{
	for (int index = 0; index < nItems; index++){
		if (items[index].id == nItemType){
			CCtrlData* pData = NewData(items[index].ctrlType, items[index].str);
			m_datas.AddTail(pData);
		}
	}

}

void COutlineItem::UpdatePosition(COutlineItem* pInsAfter)
{
	if (pInsAfter != NULL){
		m_position.top += pInsAfter->GetPosition().bottom;
		m_position.bottom += pInsAfter->GetPosition().bottom;
	}
	if (m_nItemType == ITEM_LAW)//drop a little
	{
		m_position.top += STAGE_SPACING;
		m_position.bottom += STAGE_SPACING;
	}

	if (m_nItemType >= ITEM_SYSPROP && m_nItemType <= ITEM_CHKSOLV){
		m_position.left += INDENT_WIDTH;
		m_position.right += INDENT_WIDTH;
	}

}
/*
BOOL COutlineItem::AddControls(int nItemType, CHiLevelVw* pView)
{
	for (int index = 0; index < nItems; index++){
		if (items[index].id == nItemType){
			CItemCtrl* pCtrl = AddControl(pView, items[index].ctrlType, items[index].str);
			//commented out below cause want all enabled  ???not sure what Kurt will want
	//		if ( (nItemType != ITEM_SYSPROP) )
	//			pCtrl->Enable(FALSE);
		}
	}
	return TRUE;
	
}*/

CCellCtrl* COutlineItem::ReplaceCellText(CString str)
{
	for (int i=0; i<m_controls.GetSize(); i++)
	{
		CItemCtrl* pCtrl = m_controls[i];
		if (pCtrl->IsKindOf(RUNTIME_CLASS(CCellCtrl))){
			CCellCtrl* pCell = (CCellCtrl*)pCtrl;
			pCell->SetText(str);
			pCell->Invalidate();
			return pCell;
		}
	}
	return NULL;
}

CString COutlineItem::GetPrintDef()
{
	CString strDef;
	for (int i=0; i<m_controls.GetSize(); i++)
	{
		CItemCtrl* pCtrl = m_controls[i];
		if (pCtrl->IsKindOf(RUNTIME_CLASS(CItemBtn)))
			continue;
		strDef = strDef + " " + pCtrl->m_strText;
	}
	return strDef;


}

CString COutlineItem::GetTipDef()
{
	CString strDef;
	UINT nID = IDS_ITEMUNKNOWN;
	switch (m_nItemType)
	{
	case ITEM_LAW: nID = IDS_ITEMLAW; break;
	case ITEM_SYSPROP: nID = IDS_ITEMSYSPROP; break;
	case ITEM_DIRECTION: nID = IDS_ITEMDIRECTION; break;
	case ITEM_CHKSOLV: nID = IDS_ITEMCHKSOLV; break;
	}
	strDef.LoadString(nID);
	if (strDef.GetLength() > 80)//buffer is 80
		strDef = strDef.Left(80);
	return strDef;


}


CCellCtrl* COutlineItem::GetCell()
{
	for (int i=0; i<m_controls.GetSize(); i++)
	{
		CItemCtrl* pCtrl = m_controls[i];
		if (pCtrl->IsKindOf(RUNTIME_CLASS(CCellCtrl))){
			CCellCtrl* pCell = (CCellCtrl*)pCtrl;
			return pCell;
		}
	}
	return NULL;
}

CMenuGrid* COutlineItem::GetFocusedGrid()
{
	for (int i=0; i<m_controls.GetSize(); i++)
	{
		CItemCtrl* pCtrl = m_controls[i];
		if (pCtrl->IsKindOf(RUNTIME_CLASS(CMenuGrid))){
			CMenuGrid* pGrid = (CMenuGrid*)pCtrl;
			if (pGrid->IsFocused())
				return pGrid;
		}
	}
	return NULL;
}


CGridCtrl* COutlineItem::GetGrid()
{
	for (int i=0; i<m_controls.GetSize(); i++)
	{
		CItemCtrl* pCtrl = m_controls[i];
		if (pCtrl->IsKindOf(RUNTIME_CLASS(CGridCtrl))){
			CGridCtrl* pGrid = (CGridCtrl*)pCtrl;
			return pGrid;
		}
	}
	return NULL;
}



CItemCtrl* COutlineItem::AddControl(CHiLevelVw* pView, char ch, CString str)
{
	
	CItemCtrl* pCtrl;
	int baseId;

	switch (ch){
		case 'b': pCtrl = new CItemBtn(); baseId = ID_BTNBASE; break;
		case 'c': pCtrl = new CCellCtrl(); baseId = ID_CELLBASE; break;
		case 's': pCtrl = new CItemStc(); baseId = ID_STCBASE; break;
		case 'g': pCtrl = new CMenuGrid(); 	baseId = ID_GRIDBASE; break;
	}
	// Find position for new control
	CPoint pos;
	int nLastCtrl = m_controls.GetSize() - 1;// get index of last control
	if (nLastCtrl >= 0)
	{//new controls position is to right of last control 
		CItemCtrl* pLastCtrl = m_controls[nLastCtrl];
		pos = CPoint((pLastCtrl->GetPosition().right + SPACE_BETWEEN_CTRLS), 
							pLastCtrl->GetPosition().top);
	}
	else{//if no controls, position is at outlineitem left
		pos = CPoint(m_position.left, m_position.top);
	}
	//Create new controls id, created by item type + number of control*10 +
	//stage identifier * 100  + baseID;
	int nCtrl = m_controls.GetSize() + 1;
	int stagePart = 0;
	if (GetStage())
		stagePart = GetStage()->m_nId*100;

	int ctrlId = m_nItemType + (nCtrl*10) + stagePart + baseId;
	if (m_nItemType == ITEM_LAW)
		str = GetStage()->m_pPrinc->GetDisplayString();

	pCtrl->m_strText = (str);
	pCtrl->SetOutlineItem(this);
	//Create the control
	pCtrl->Create(pView, ctrlId, pos);
	//Update outline items position
	m_position.right = SPACE_BETWEEN_CTRLS + pCtrl->GetPosition().right;
	m_controls.Add(pCtrl);//Add to list of controls
//	if ((ch == 'g') || (ch == 'c'))
//		GetStage()->m_controls.AddTail(pCtrl);
	return pCtrl;

}

COutlineItem* CStageObj::InsertTableItem()
{
	COutlineItem* pItem = new COutlineItem(ITEM_TABLE);
	pItem->m_pStage = this;
	int nInsPos = 2 + m_tables.GetCount();
	COutlineItem* pInsAfter = m_items[nInsPos - 1];
	int nHeight = pInsAfter->m_position.Height();
	int nWidth = INDENT_WIDTH;
	if (!m_tables.IsEmpty())
	{
		nWidth = 0;
	}
	CRect rcPos = pInsAfter->m_position;
	rcPos.OffsetRect(nWidth, (VERTICAL_SPACING + nHeight));
	pItem->SetPosition(rcPos);
	m_items.InsertAt(nInsPos, pItem);

	
	return pItem;
}

CTableGrid* COutlineItem::AddTable(int nID)
{
	CTableGrid* pTable = new CTableGrid(nID);
	pTable->SetOutlineItem(this);
	m_controls.Add(pTable);
	CCtrlData* pData = NewData(pTable->GetChar(), "");
	pData->m_nTableId = nID;
	m_datas.AddTail(pData);
	return pTable;

}

CCtrlData* COutlineItem::NewData(char chType, CString strLabel)
{
	CCtrlData* pData = new CCtrlData();
	pData->m_chType = chType;
	pData->m_strLabel = strLabel;
//	pData->m_bEnabled = TRUE;
	pData->m_bInitState = TRUE;
	pData->m_pItem = this;
	pData->m_pDocument = GetStage()->m_pDocument;
	return pData;

}


CTableGrid* COutlineItem::ReadInTable(CCtrlData* pData)
{
	CStageObj* pStage = GetStage();
	CTableRow* pProp; 
	CTableGrid* pTable = NULL;

	if (!pData->m_properties.IsEmpty())//if have properties
	{//Create the table with that id
		pTable = new CTableGrid(pData->m_nTableId);
		pTable->SetOutlineItem(this);
		m_controls.Add(pTable);//add to list of controls
		pTable->Initialize();
		pStage->m_tables.AddTail(pTable);//Add to list of tables

	}
	while (!pData->m_properties.IsEmpty())
	{
		pProp = (CTableRow*)pData->m_properties.RemoveHead();//remove from serialization list
		pProp->m_pTable = pTable;
		pProp->CheckObject();
		pTable->AddProperty(pProp);//add to control's list (handles deletion)

	}
	if (pTable != NULL)//should never happen
	{
		pTable->AutoSize();
	//A table is initially expanded when created
//	pTable->m_bCollapsed = FALSE;

//	if (!pData->m_bEnabled)//However, if the table was not saved in its enabled state
//		pTable->ToggleCollapse();//we must collapse it.
//		pStage->EnableBtn(pData->m_bEnabled, IDB_BTN_DELPROP);
	}
	return pTable;


}


void COutlineItem::UpdateCtrlsthatFollow(CItemCtrl* pCtrl)
{
	for (int i=0; i<m_controls.GetSize(); i++)
	{
		CItemCtrl* pCtrlInList = m_controls[i];
		if (pCtrlInList->GetId() == pCtrl->GetId()){
			for ( int j=i+1; j<m_controls.GetSize(); j++){
				CItemCtrl* pNextCtrl = m_controls[j];
				int xDist = pCtrl->GetPosition().right + 5 - pNextCtrl->GetPosition().left;
				pNextCtrl->UpdatePosition(xDist, 0);
				pCtrl = pNextCtrl;
			}
		}
	}

}

void COutlineItem::UpdateDatas()
{
	// Empty data list
	int i = 0; 
	POSITION pos = m_datas.GetHeadPosition();
	while (pos != NULL)
	{
		CCtrlData* pData = m_datas.GetNext(pos);
		pData->m_properties.RemoveAll();
		if 	(i < m_controls.GetSize())
		{
			CItemCtrl* pCtrl = m_controls[i];
			i++;
			pData->Update(pCtrl);
		}
	}

}


int COutlineItem::HitTest(CPoint point)
{
   	ASSERT_VALID(this);
   	

   	CRect normPos = m_position;
   	normPos.NormalizeRect();
   	if (point.x >= normPos.left && point.x <= normPos.right &&
   		point.y >= normPos.top && point.y <= normPos.bottom)
   		return 1;
   	return 0;
}


CItemCtrl* COutlineItem::CtrlAt(const CPoint &point)
{
		for (int j=0; j<m_controls.GetSize();  j++)
		{
			CItemCtrl* pCtrl = m_controls[j];
			if 	(pCtrl->HitTest(point))
				return pCtrl;
		}


	return NULL;

}

/////////////////////////////////////////////////////////////////////////////
// CPrincItem -- common base for Principle tree item nodes (principles, substeps)
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CPrincItem, CObject, VERSIONABLE_SCHEMA |3);
CPrincItem::~CPrincItem()
{
	// free any subitems (recurses through subitem destructors). 
	RemoveAllSubItems();
}

void CPrincItem::RemoveAllSubItems()
{
	while (! m_subItems.IsEmpty())
		delete m_subItems.RemoveHead();
}

void CPrincItem::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;

	CObject::Serialize(ar);	// doesn't really do anything
  	if (ar.IsStoring())
   	{
		/* ar << m_strId  // deleted v3 */ 
		ar << m_bChecked;
		// added v2:
		ar << (WORD) m_status;
		// add v3:
		ar << (WORD) m_checkStatus;
	} else {
		WORD wTemp;
		if (nClassVersion < 3) {
			CString strJunkId;
			ar >> strJunkId;	// consume unused id and throw it away
		}
		ar >> m_bChecked;
		if (nClassVersion >= 2) {
			ar >> wTemp; m_status = (Status) wTemp;
		} // else leave unknown from constructor
		if (nClassVersion >= 3) {
			ar >> wTemp; m_checkStatus = (Status) wTemp;
		} // else leave unknown from constructor
	}
	m_subItems.Serialize(ar);
/* not needed since m_pParent serialized in CPrincStep
	// on loading, fix up back pointer to parent for subitems (wasn't serialized)
	if (ar.IsLoading() && HasSubItems()) {
		for (POSITION pos = m_subItems.GetHeadPosition(); pos != NULL; ) {
			CPrincItem* pChild = m_subItems.GetNext(pos);
			pChild->m_pParent = this;
		}
	} */
}

/////////////////////////////////////////////////////////////////////////////
// CPrincObj
//////////////////////////////////////////////////////////////////////////
// 
// Represents a law we are applying in the abstract algebraic solution
//
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CPrincObj, CPrincItem, VERSIONABLE_SCHEMA | 2);

// Constant strings used for laws:
const char CPrincObj::szKinematics[] ="Kinematics";
const char CPrincObj::szNewtonsFirst[] = "Newton's First Law";
const char CPrincObj::szNewtonsSecond[] =  "Newton's Second Law";
const char CPrincObj::szConsOfEnergy[] = "Conservation of Energy";
const char CPrincObj::szNetWork[] = "Net Work";

CPrincObj::CPrincObj(BOOL bCreateStage /*=FALSE*/)
{
	m_pDocument = NULL;		// starts off unattached
	
	if (bCreateStage) {
		m_pStageObj = new CStageObj();
		m_pStageObj->m_pPrinc = this;
	} else {
		// Stage pointer starts off empty. Stage should be loaded by serialization
		// or constructed and attached by caller.
		m_pStageObj = NULL;
	}
}

CPrincObj::~CPrincObj()
{
	delete  m_pStageObj;
}
 
void CPrincObj::Serialize(CArchive& ar)
{
	// If loading, save class version over base class load
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	
	// Special case to load old V1 items, before CPrincItem base class
	if (ar.IsLoading() && nClassVersion == 1) {
		LoadV1(ar);
		return;
	}

	// Version 2: Serialize base class info adding class info with version.
	ar.SerializeClass(RUNTIME_CLASS(CPrincItem));
	CPrincItem::Serialize(ar);

  	if (ar.IsStoring()) {	// version 2 (after CPrincItem base)
		ar << m_strLaw;
		ar << m_pStageObj;
	} else {
		m_pDocument = (CFBDDoc*) ar.m_pDocument;
    	ASSERT_VALID(m_pDocument);
    	ASSERT_KINDOF(CFBDDoc, m_pDocument);
		
		ar >> m_strLaw;
		ar >> m_pStageObj;
	}
	m_listBodies.Serialize(ar);
}

// load old version 1 PrincObj, before moved stuff to PrincItem base
void CPrincObj::LoadV1(CArchive &ar) 
{
	// if (ar.IsStoring()){ // version 1 storing code
	//	ar << m_strId;
	//	ar << m_strLaw;
	//	ar << m_pStageObj;
	//	ar << m_bChecked;
	// } else {
		m_pDocument = (CFBDDoc*) ar.m_pDocument;
    	ASSERT_VALID(m_pDocument);
    	ASSERT_KINDOF(CFBDDoc, m_pDocument);
		CString strJunkId;
		ar >> strJunkId;
		ar >> m_strLaw;
		ar >> m_pStageObj;
		ar >> m_bChecked;
	// }
	m_listBodies.Serialize(ar);

	// Do a SetLaw to add substeps where needed, since v1 didn't have them
	SetLaw(m_strLaw);
}

/////////////////////////////////////////////////////////////////////////////
// CPrincObj operations
////////////////////////////////////////////////////////////////////////////
void CPrincObj::SetBodies(const CStringList &bodies) // set from string list
{
	m_listBodies.RemoveAll();	// empty existing contents

	POSITION pos = bodies.GetHeadPosition();
	while (pos != NULL) {
		CString strBody = bodies.GetNext(pos);
		m_listBodies.AddTail(strBody);
	}
}

CString CPrincObj::GetBodyStr() const// Get body list as space-delimited string
{
	//something very screwy is going on with these CStrings
	//obviously having to do with using a stringlist which returns
	//a pointer to a string (i think)
	//Anyway, strBody is rationed the same 
	//address (and fillings) it was given in onNewPrinciple.  If I am concatenating
	//I get a double string.
	//Format seems to solve the problem (not copying string and its pointer);
	CString strBodyList;
	POSITION posBody = m_listBodies.GetHeadPosition();
	while (posBody != NULL) {
		CString strBody = m_listBodies.GetNext(posBody);
		CString strBodySep; 
		strBodySep.Format("%s ", strBody); // append space separator
		strBodyList = strBodyList + strBodySep;
	}
	strBodyList.TrimRight();

	return strBodyList;
}

CString CPrincObj::GetDisplayString()
{
	CString strPrinc;
	CString strBodyList = GetBodyStr();
	strPrinc.Format("Applying %s on %s", m_strLaw, strBodyList);

	return strPrinc;
}

CString CPrincObj::GetHelpSysName()	// Gets Law Id used by help system
{
	if (m_strLaw == szKinematics)
		return "kinematics";
	else if (m_strLaw == szNewtonsFirst)
		return "newtons-first";
	else if (m_strLaw == szNewtonsSecond)
		return "newtons-second";
	else if (m_strLaw == szConsOfEnergy)
		return "cons-of-energy";
	else if (m_strLaw == szNetWork)
		return "net-work";
	else {
		TRACE("GetHelpSysName -- unknown law %s\n", m_strLaw);
		return m_strLaw;	// !!! Lisp bombs if contains spaces
	}
}

// helper to add a substep
CPrincStep* CPrincObj::AddStep(const CString& strText)
{
	// Create new step. 
	CPrincStep* pSubStep = new CPrincStep(strText);
	
	// add to subitem list with back pointer to parent.
	AddSubItem(pSubStep);

	return pSubStep;
}

void CPrincObj::SetLaw(LPCTSTR szLawName)
{
	m_strLaw = szLawName;	// could check it is one of our law names

	// remove any existing substeps for prev law
	RemoveAllSubItems();
	
	// Add any substeps required to apply this law.
	// Currently this is a special case for Newton's law(s) only: 
	if (m_strLaw == CPrincObj::szNewtonsSecond ||
		m_strLaw == CPrincObj::szNewtonsFirst) {
		AddStep(CPrincStep::szFBD);
		AddStep(CPrincStep::szEquations);
	}

}

// Update state on change in child items
void CPrincObj::OnChildStateChange()
{
	// save current state to see if it changes
	BOOL bOldCheck = m_bChecked;

	BOOL bAllStepsDone = TRUE;	  // until find counter instance
	POSITION pos = m_subItems.GetHeadPosition();
	while (pos != NULL) {
		CPrincItem* pChild = m_subItems.GetNext(pos);
		if (! (pChild->m_bChecked && pChild->m_checkStatus == statusCorrect)) {
			// !!! what if children all checked but unknown?
			bAllStepsDone = FALSE;
			break;
		}
	}
	m_bChecked = bAllStepsDone;
	m_checkStatus = bAllStepsDone ? statusCorrect : statusUnknown;
	
	// Broadcast update on check state change. (check state is "done button" 
	// used by Hi-Level view to enable/disable stage items).
	if (bOldCheck != m_bChecked && m_pDocument) {
		m_pDocument->UpdateAllViews(NULL, HINT_CHKCHANGE_PRINC, this);
	}
}

void CPrincObj::CheckObject()
{
	CString strLawName = GetHelpSysName();
	CString strStageId = GetStageId();
	CString strBodies = GetBodyStr();
	LPCSTR pszResult = HelpSystemExecf("(lookup-principle %s (%s) %s)", 
					strLawName, strBodies, strStageId); 
	CCheckedObj::ApplyStatus(pszResult, m_status, m_errors);
}

/////////////////////////////////////////////////////////////////////////////
// CPrincStep  -- substep in applying a principle
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CPrincStep, CPrincItem, VERSIONABLE_SCHEMA | 1);
// some test files have obsolete version 1 was subclass of CPrincObj

const char CPrincStep::szFBD[] ="Draw Free Body Diagram";
const char CPrincStep::szEquations[] = "Write Equations";

CPrincStep::~CPrincStep()
{
}

void CPrincStep::Serialize(CArchive& ar)
{
	// On loading, save version number over base class serialization
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	
	// Serialize base class info adding class info with version.
	ar.SerializeClass(RUNTIME_CLASS(CPrincItem));
	CPrincItem::Serialize(ar);

  	if (ar.IsStoring()) {//started with version 1
		ar << m_pParent;
		ar << m_strText;
	} else {
		ar >> m_pParent;
		ar >> m_strText;
	}
}

CString CPrincStep::GetStepId() // map our display name to help system name
{
	if (m_strText == szFBD)
		return "fbd";
	else if (m_strText == szEquations)
		return "equations";
	else {
		TRACE("GetStepId -- unknown step %s\n", m_strText);
		return m_strText;		// !!! Lisp bombs if contains spaces
	}
}





