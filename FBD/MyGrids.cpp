///////////////////////////////////////////////////////////////////////////
//
// MyGrids.cpp
//
#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"
#include "Helpifc.h"
#include "FormulaDlg.h"
#include "VecDlg.h"
#include "VecAVDlg.h"
#include "VariableDlg.h"
#include "LabRadDlg.h"
#include "PropertyDlg.h"
#include "InPlaceList.h"
#include "MyGrids.h"
#include "HiLevelVw.h"
#include "StageObj.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


////////////////////////////////////////////////////////////////////////////////////////
//CTableRow
////////////////////////////////////////////////////////////////////////////////////////
typedef struct
{
	char* strText;
	char* strHelp;
} planString;

planString Choices[] = {

 "Sought",						"sought",
 "Unknown",						"unknown",
 "Known",						"known",
 "Known by formula",			"known-by-formula",
 "Known by equivalent quantity",	"known-by-equivalent-quantity",
};

const int numChoices ARRAY_SIZE(Choices);


IMPLEMENT_SERIAL(CTableRow, CVariable, VERSIONABLE_SCHEMA | 1)

CTableRow::CTableRow()
{
	m_pTable = NULL;
}

CTableRow::~CTableRow()
{}

void CTableRow::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	// serialization of base class descriptor
	CVariable::Serialize(ar);
	SerializeLists(ar);
	if (ar.IsStoring())
	{						 // store object specific data
	}
	else{
	}
}

void CTableRow::SerializeLists(CArchive &ar)
{
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;
	if (ar.IsStoring()){
		int size = m_images.GetCount();
		ar << (DWORD)size;
		POSITION pos = m_images.GetHeadPosition();
		for (int i =0; i < size; i++)
			ar << (DWORD)m_images.GetNext(pos);
		
		size = m_strings.GetCount();
		ar << (DWORD)size;
		pos = m_strings.GetHeadPosition();
		for (i =0; i < size; i++)
			ar << m_strings.GetNext(pos);
	}
	else
	{
		DWORD size;
		ar >> size;
		DWORD index;
		if (size != 0){	
			for (UINT i =0; i<size; i++){
					ar >> index;
					m_images.AddTail(index);
			}
	
		}
		ar >> size;
		CString str;
		if (size != 0){	
			for (UINT i =0; i<size; i++){
					ar >> str;
					m_strings.AddTail(str);
			}
	
		}

	}
	
}
///////////////////////////////////////////////////////////////////////////////////////

CDrawObj* CTableRow::Clone()
{
	// create duplicate w/same properties 
	CTableRow* pClone = new CTableRow();

	pClone->m_strId = m_strId;	// Initially, use same id, if added to document, 
								// different id will be generated

	pClone->m_strName = m_strName;
	pClone->m_status = m_status;

	pClone->m_nType = m_nType;
	pClone->m_strForceType = m_strForceType;
	pClone->m_strValue = m_strValue;
	pClone->m_strObject = m_strObject;
	pClone->m_strAgent = m_strAgent;
	pClone->m_strTime = m_strTime;
    	
	pClone->m_strDef = m_strDef;
	pClone->m_pTable = m_pTable;
  
    //Needed to add because checking temporary cloned object which may refer to its
	//m_pDocument
   	pClone->m_pDocument = m_pDocument;
	return (pClone);
}

CDialog* CTableRow::GetPropertyDlg()
{
	if (m_nType == ID_VARIABLE_ADDFORCE)
		return new CVectorDlg(this);

	else if (m_nType == ID_VARIABLE_ADDVELOCITY ||
				m_nType == ID_VARIABLE_ADDDISPLACEMENT	||
			 m_nType == ID_VARIABLE_ADDACCELERATION ||
			 m_nType == ID_VARIABLE_ADDSPEED ) 
	{
		CVectorMoveDlg* pDlg = new CVectorMoveDlg(this);
		if (m_nType == ID_VARIABLE_ADDVELOCITY)
			pDlg->m_strDescription = "Velocity";
		else if (m_nType == ID_VARIABLE_ADDACCELERATION)
			pDlg->m_strDescription = "Acceleration";
		else if (m_nType == ID_VARIABLE_ADDDISPLACEMENT)
			pDlg->m_strDescription = "Displacement";
		else 
			pDlg->m_strDescription = "Speed";
		return pDlg;
	}

	else if (m_nType == ID_VARIABLE_ADDRADIUS)
		return new CRadiusDlg(this);

	else if (m_nType == ID_VARIABLE_ADDMASS ||
			m_nType == ID_VARIABLE_ADDTIME	)
		return new CVariableDlg(this);

	else
		return new CPropertyDlg(this);
}

BOOL CTableRow::OnEditProperties()
{
	CDrawObjDlg* pDlg = (CDrawObjDlg*)GetPropertyDlg();				//dialog to specifically define our chosen proerty
	CHiLevelVw* pView = theApp.GetHiLevelVw();
	//GetThis from view
	//can't get from back pointer to the table because might not be created yet

	//Dialog will contain body lists.
	POSITION pos = m_pTable->GetStage()->m_pPrinc->m_listBodies.GetHeadPosition();
	while (pos != NULL)
	{
		CString tempstr = m_pTable->GetStage()->m_pPrinc->m_listBodies.GetNext(pos);
		pDlg->m_strPlanBodies.AddTail(tempstr);

	}
	pDlg->m_bProp = TRUE;

//	int id = nType + (pStage->m_nId * 100);

	//if the table of that type already exists, find it
//	CTableGrid* pTable = pStage->GetTable(id);
//	pDlg->m_pTable = pTable;

   	int nResult = pDlg->DoModal();	
   	delete pDlg;
    
   	if (nResult != IDOK)	// user cancelled dialog
   		return FALSE;
	   	// Changing props makes document dirty:
   	if (m_pDocument != NULL) 
   		m_pDocument->SetModifiedFlag();
   
	// After change, status is unknown until rechecked
	m_status = statusUnknown;
    
  	// invalidate object in case change has visible effect (e.g. label, status)
   	Invalidate();
    
	// Now check updated asserted properties with help system. This may update status
	NotifyChange();

   	return TRUE;

}

void CTableRow::NotifyDelete()
{
	CHiLevelVw* pView = theApp.GetHiLevelVw();
	//GetThis from view
	//can't get from back pointer to the table because might not be created yet
	int id;
	if (pView->m_pFocusedItem)
		id = pView->m_pFocusedItem->GetStage()->m_nId;

	CString stageId;
	stageId.Format("stage-%d", id);

	(void)HelpSystemSendf("(delete-plan-stage-object %s %s)", 
			STR2ARG(m_strId),
				STR2ARG(stageId) );

}

void CTableRow::NotifyChange()
{
	NotifyDelete();
	CheckObject();
}

void CTableRow::CheckObject()
{
	LPCTSTR pszResult;
	CHiLevelVw* pView = theApp.GetHiLevelVw();
	//GetThis from view
	//can't get from back pointer to the table because might not be created yet
	int id;
	if (pView != NULL){
		if (pView->m_pFocusedItem)
			id = pView->m_pFocusedItem->GetStage()->m_nId;
	}
	else if (m_pTable != NULL)
		id = m_pTable->GetStageID();

	CString stageId;
	stageId.Format("stage-%d", id);

	CString strTime = m_strTime;
	CString strObject = m_strObject;
	CString strValue = m_strValue;
		//This is kind of a cheat.  However, I don't know where else to put this...
	if (m_strValue.Find("duration")==0){
		strValue = "duration";
		strTime.Format("%s to %s", m_strObject, m_strTime);
		strObject = "";
	}

	pszResult =
		HelpSystemExecf( "(lookup-plan-stage-quantity |%s| |%s| %s |%s| %s %s %s)",
			STR2ARG(m_strForceType),//used to hold force type (instantaneous or average)
			STR2ARG(strValue),
			STR2ARG(strObject),
			STR2ARG(strTime), /* pTimeNum, */
			STR2ARG(m_strAgent),
			STR2ARG(m_strId),
			STR2ARG(stageId)
			);
//	CGridCell* pCell = GetCell(m_properties.GetCount(), 0);
//	Status oldStatus = pCell->m_status;
	// Translate Lisp result into new status
	// Note piggybacked command might execute here!
// 	ApplyStatus(pszResult, pCell->m_status);
	Status oldStatus = m_status;
	// Translate Lisp result into new status
	// Note piggybacked command might execute here!
	CStringList strErrs;
 	ApplyStatus(pszResult, m_status, strErrs);
	
 	// redraw if status changed.
 //	if (m_status != oldStatus)
//		Invalidate(); 
	
 	// redraw if status changed.
// 	if (pCell->m_status != oldStatus)
//		Invalidate(); 
	return;

}

int CTableRow::SetType(int nID)
{
	int nType;
	//to keep tables unique, but able to pull out type
	if (nID < ID_ADDPROP_KINEMATICFIRST){
		nType = 0 + IDG_TABLE_FIRST ;
		m_nType = nID - ID_ADDPROP_FIRST + ID_VARIABLE_ADDFIRST;
	}
	else if ((nID >= ID_ADDPROP_KINEMATICFIRST)&&(nID <= ID_ADDPROP_KINEMATICLAST)){
		nType = 1 + IDG_TABLE_FIRST;
		if (nID == ID_ADDPROP_TIME)
			m_nType = ID_VARIABLE_ADDTIME;
		else
			m_nType = nID - ID_ADDPROP_FIRST + ID_VARIABLE_ADDFIRST;
	}
	else{
		nType = 2 + IDG_TABLE_FIRST;
		if (nID == ID_ADDPROP_MASS)
			m_nType = ID_VARIABLE_ADDMASS;
		else if (nID == ID_ADDPROP_RADIUS)
			m_nType = ID_VARIABLE_ADDRADIUS;
		else
			m_nType = nID;
	}
	return nType;

	
}
////////////////////////////////////////////////////////////////////////////////////////
//CTableGrid
////////////////////////////////////////////////////////////////////////////////////////

#define CTRL_HEIGHT	20

IMPLEMENT_DYNCREATE(CTableGrid, CGridCtrl);


CTableGrid::CTableGrid()
{
	m_bHighlight = FALSE;
	m_bOldEnabledState = TRUE;
//	m_bCollapsed = FALSE;
}

CTableGrid::CTableGrid(int nID)
{
	m_bHighlight = FALSE;
	m_bOldEnabledState = TRUE;
//	m_bCollapsed = FALSE;
	m_nId = nID;
}

BOOL CTableGrid::Create(CWnd* pParentWnd, int ctrlID, CPoint pos)
{
            
	m_position = CRect(CPoint(pos.x, pos.y), CSize(25, CTRL_HEIGHT));
	m_nId = ctrlID;

	BOOL bResult =  CGridCtrl::Create(m_position, pParentWnd, m_nId);
	SetColumnCount(3);
	SetListMode();
	SetHeaderSort(FALSE);
	CImageList* pImgList = new CImageList();
	pImgList->Create(IDR_PLAN, 15, 1, RGB(192, 192, 192));//create image list
	SetImageList(pImgList);
	MoveControl(m_position);

	return bResult;
	
}

void CTableGrid::Initialize()
{
	CPoint pt = CPoint(m_pItem->m_position.left, 
		(m_pItem->m_position.top + SPACE_BETWEEN_CTRLS));
	Create(GetStage()->m_pView, GetId(), pt);
	MoveControl(GetPosition());
	AddHeader(GetId());
}


CTableGrid::~CTableGrid()
{
	if (m_pImageList)
		delete m_pImageList;

	// Empty data list
	while (!m_properties.IsEmpty()){
		CTableRow* pProp = m_properties.RemoveHead();
		delete pProp;
	}


}

BEGIN_MESSAGE_MAP(CTableGrid, CGridCtrl)
	//{{AFX_MSG_MAP(CTableGrid)
	//}}AFX_MSG_MAP
	ON_CONTROL_REFLECT_EX(BN_CLICKED, OnClicked)
END_MESSAGE_MAP()
///////////////////////////////////////////////////////////////////////////////////////////////
//
void CTableGrid::FillListItems(int nCol, LPARAM cltList)
{
	CListBox* pList = (CListBox*)cltList;

	if ((nCol == 1)||(nCol == 2))
	{
		for (int i = 0;  i < numChoices-1; ++i) 
			pList->AddString(Choices[i].strText);//make sure in same order as in CExpDlg.h
		if (nCol == 1)//only magnitude can be equal to another quantity
			pList->AddString(Choices[i].strText);

		((CInPlaceList*)pList)->m_bHasIcons = TRUE;
//			((CInPlaceList*)pList)->m_bHasMenu = TRUE;

	}
	
}

void CTableGrid::Highlight(BOOL bHighlight)
{
	Deselect();
	UpdateWindow();

	m_bHighlight = bHighlight;

	int nCols = GetColumnCount();//get number of columns
	if (nCols < 3) return;//if only two cols, no direction column

	for (int nRow = 0; nRow < GetRowCount(); nRow++)
	{
		CGridCell* pCell = GetCell(nRow, 2);//get direction cell for this row
		if (!pCell->IsSelectable())//if no direction (for example, time elapsed has none)
		{
			for (int col = 0; col < nCols - 1; col++){
				CGridCell* pCell = GetCell(nRow, col);
				pCell->EnableSelection(!bHighlight);//do not select row
			}
		}
	}

	if (bHighlight)
		SetTextBkColor(RGB(255, 255, 0));
	else
		SetTextBkColor(RGB(255, 255, 255));

    for (int row = 1; row < GetRowCount(); row++)
        RedrawRow(row);


}
//************************************************************************
//Helper functions to initialize tables.
//************************************************************************

//Adds tables Header row
void CTableGrid::AddHeader(int nId)
{

	if ((GetCtrlID() == IDG_FORCEPROP) || (GetCtrlID() == IDG_KINEMATICPROP))
	{
		if (GetCtrlID() == IDG_FORCEPROP)
			InsertRow("Forces on system");
		else 
			InsertRow("Kinematic properties of system");
		SetItemText(0, 1, "Magnitude");
		SetItemText(0, 2, "Direction");
	}
	else{
		InsertRow("Other system properties");
		SetItemText(0, 1, "Value");
		DeleteColumn(2);
	}
	//first row (header row) (row 0) is read only
	SetFixedRowCount(1);

	for (int nCol=0; nCol<GetColumnCount(); nCol++){
		CGridCell* pCell = GetCell(0, nCol);
		pCell->EnableSelection(FALSE);
	}
	AutoSize();

}

//all tables:  first column can be edited via dlg, 2nd & 3rd columns have drop down menus
int CTableGrid::AddRow(CString strForce)
{
	int	row = InsertRow(strForce);
	SetColumnType(0, GVET_EDITBOX);
	SetColumnType(1, GVET_LISTBOX);
	if (GetColumnCount() > 2)
		SetColumnType(2, GVET_LISTBOX);
	AutoSize();
		
	return row;
}

void CTableGrid::AddProperty(CTableRow* pProp)
{
	m_properties.AddTail(pProp);
	pProp->m_pTable = (CTableGrid*)this;

	int nRow = AddRow(pProp->m_strDef);
	CGridCell* pCell = GetCell(nRow, 0);
	pCell->lParam = (LONG)pProp;
	pCell->m_status = pProp->m_status;

	for (int nCol=1; nCol <GetColumnCount(); nCol++)
	{
		CGridCell* pCell = GetCell(nRow, nCol);
		if (GetCtrlID() != IDG_OTHERPROP){
			if (nCol == 1)
				pCell->szMagDir = "Mag";
			else if (nCol == 2)
				pCell->szMagDir = "Dir";
		}

		pCell->lParam = (LONG)pProp;

		int nImage = -1;
		CString strKnownStatus;
		if (!pProp->m_images.IsEmpty())
			nImage = pProp->m_images.RemoveHead();
		
		if (nImage == 0)
			strKnownStatus = "sought";
		else if (nImage == 1)
			strKnownStatus = "unknown";
		else if (nImage == 2)
			strKnownStatus = "known";

		CString str;
		if (!pProp->m_strings.IsEmpty())
			str = pProp->m_strings.RemoveHead();

		CTableRow* pExProp = GetMatchingProp(str);
		if (pExProp != NULL)
			strKnownStatus = "known-by-equivalent-quantity";
		
		if ((strKnownStatus.IsEmpty() && !str.IsEmpty()))
			strKnownStatus = "known-by-formula";
			
		SetItemImage(nRow, nCol, nImage);
		SetItemText(nRow, nCol, str);
		if ( (nImage != -1) || (!str.IsEmpty()) )
			pCell->CheckObject();
		//	CheckCell(nRow, nCol, strKnownStatus);
	}
	if (pProp->m_strValue.Find("time") != -1)
	{
		SetItemState(nRow, 2, GVIS_READONLY);
		pCell = GetCell(nRow, 2);
		pCell->EnableSelection(FALSE);//do not allow selection of last column (time has no direction)
	}

	SelectRow(nRow);



}

void CTableGrid::DeleteProperty()
{
	CString stageId;
	stageId.Format("stage-%d", GetStageID());

	int nRow = GetFocusCell().row;
	if (nRow == -1)
		return;
	CGridCell* pCell = GetCell(nRow, 0);
	CTableRow* pProp = (CTableRow*)pCell->lParam;
	if (DeleteRow(nRow))
	{
		RemoveProperty(pProp);
		pProp->NotifyDelete();
		pProp->Delete();
	}
}

void CTableGrid::RemoveProperty(CTableRow* pProp)
{
	POSITION pos = m_properties.Find(pProp);
	m_properties.RemoveAt(pos);//Delete from list of properties
	GetStage()->m_pDocument->SetModifiedFlag(TRUE);
}

void CTableGrid::OnEndEditCell(int nRow, int nCol, CString str)
{
	CGridCell* pCell = GetCell(nRow, nCol);

	if (!str.IsEmpty()){
		int nImage = -1;
		CString tempStr;
		if (str == "Known by equivalent quantity")
		{

			CTableRow* pProp = new CTableRow();
			CTableRow* pOldProp = (CTableRow*)pCell->lParam;

			pProp->m_nType = pOldProp->m_nType;
			CDialog* pDlg = pProp->GetPropertyDlg();				//dialog to specifically define our chosen proerty
//			((CDrawObjDlg*)pDlg)->m_strPlanBdy = m_pItem->GetStage()->m_strBody;
			((CDrawObjDlg*)pDlg)->m_bProp = TRUE;
			if (pDlg->IsKindOf(RUNTIME_CLASS(CVectorMoveDlg)))
				((CVectorMoveDlg*)pDlg)->m_bAddGrav = TRUE;

			if (pDlg->DoModal() != IDOK){
				delete pProp;
				delete pDlg;
				return;
			}

			if ( (!pProp->m_strForceType.IsEmpty()) && (pProp->m_strForceType[0] == 'G') )
			{//change to gravitational acceleration
				pProp->m_strValue = pProp->m_strForceType + " " + pProp->m_strValue;
				pProp->m_nType = (ID_ADDPROP_GRAVACC);
				pProp->m_strForceType.Empty();
				pProp->m_strDef = pProp->m_strValue + " of " + pProp->m_strObject ;

			}
			tempStr = pProp->m_strDef;
			int nType = pProp->SetType(pProp->m_nType);
			int id = nType + (GetStage()->m_nId * 100);//Ctrl id
			//if the table of that type already exists, find it
			CTableGrid* pTable = GetStage()->GetTable(id);
			CTableRow* pExProp = NULL;
			if (pTable){
				pExProp = pTable->GetMatchingProp(pProp->m_strDef);
			}
		
			if (pExProp == NULL){

				GetStage()->m_pView->GetDocument()->GenerateId(pProp);//generate unique ID
				pProp->CheckObject();

				//otherwise create the table
				if (pTable == NULL)
				{
					COutlineItem* pItem = GetStage()->InsertTableItem();
					pTable = pItem->AddTable(id);
					pTable->Initialize();
					GetStage()->AddTable(pTable);
				}
				pProp->m_pTable = pTable;
				pTable->AddProperty(pProp);
				COutlineItem* pItem = pTable->GetOutlineItem();
				pItem->GetStage()->UpdateItemsthatFollow(pItem);
				pItem->GetStage()->m_pView->UpdateStagesthatFollow(pItem->m_pStage);
				CDocument* pDoc = theApp.GetDocument();
				if (pDoc != NULL)
				{
					pDoc->SetModifiedFlag();
					pDoc->UpdateAllViews(NULL, HINT_UPDATE_HILEVELVW, NULL);
				}
			}
			else{
				delete pProp;
			}
			delete pDlg;
		}
		else if (str == "Known by formula")
		{
			CFormulaDlg dlgFormula;
			if (dlgFormula.DoModal() != IDOK)
				return;
			tempStr = dlgFormula.m_strFormula;
		}
		else if (str[0] == 'U')	nImage = 0;
		else if (str[0] == 'K')	nImage = 1;
		else if (str[0] == 'S')	nImage = 2;

		SetItemImage(nRow, nCol, nImage);
		SetItemText(nRow, nCol, tempStr);
		pCell->CheckObject();
		m_pItem->GetStage()->m_pView->GetDocument()->SetModifiedFlag();
	}
	
	AutoSizeColumns();
}

/*
void CTableGrid::ToggleCollapse()
{
	m_bCollapsed = !m_bCollapsed;

	if (GetCtrlID() != IDG_OTHERPROP)
	{
		//need something representative of magnitude and direction
		if (m_bCollapsed){
			SetItemText(0, 1, "MAG");
			SetItemText(0, 2, "DIR");
		}
		else{
			SetItemText(0, 1, "Magnitude");
			SetItemText(0, 2, "Direction");
		}

	}
	AutoSize();
	Enable(!m_bCollapsed);
	if (m_bCollapsed)
		Deselect();


}
*/
BOOL CTableGrid::OnClicked()
{
	//for logging purposes, can't be a log button cause created at runtime
	CString strText;
	GetWindowText(strText);

	GetParent()->m_pFocusedTable = this;
//	GetParent()->m_pFocusedItem = m_pItem;
	m_pItem->SetFocus();
		

	CStageObj* pStage = m_pItem->GetStage();
	int nRow = GetFocusCell().row;
	if (nRow < 0)
		return TRUE;

	if (IsHighlighted()){
		int nCol = GetFocusCell().col;
		if (!IsValid(nRow, nCol)) return TRUE;
		CGridCell* pCell = GetCell(nRow, 0);
		CTableRow* pProp = (CTableRow*)pCell->lParam;

		CString	str = GetItemText(nRow, 0);

		GetParent()->UpdateFromUserSelection(pProp);
		Highlight(FALSE);
		if (IsWindowEnabled())
			pStage->EnableBtn(TRUE, IDB_BTN_ADDPROP);
	}else{
		pStage->EnableBtn(TRUE, IDB_BTN_DELPROP);
	}

	return FALSE;//let parent handle as well
}



CHiLevelVw* CTableGrid::GetParent()
{
	return (CHiLevelVw*)CWnd::GetParent();
}

void CTableGrid::Deselect()
{
	ResetSelectedRange();
	SetFocusCell(-1, -1);
}

void CTableGrid::SelectRow(int nRow)
{
//	ResetSelectedRange();
	int nMaxCol = GetColumnCount() - 1;
	SetSelectedRange(nRow, 0, nRow, nMaxCol);
	CCellID cell = CCellID(nRow, 0);
	m_LeftClickDownCell = cell;
	m_SelectionStartCell = m_LeftClickDownCell;
    m_MouseMode = m_bListMode? MOUSE_SELECT_ROW : MOUSE_SELECT_CELLS;
    OnSelecting(cell);

	SetFocusCell(nRow, 0);
    GetParent()->SendMessage(WM_COMMAND, MAKELONG(GetDlgCtrlID(), BN_CLICKED), 
                                         (LPARAM) GetSafeHwnd());


}


BOOL CTableGrid::DrawCell(CDC* pDC, int nRow, int nCol, CRect rect, BOOL bEraseBk)
{
    if (!m_bAllowDraw) return FALSE;

    GV_ITEM Item;
    Item.mask = GVIF_TEXT | GVIF_FORMAT | GVIF_STATE | GVIF_IMAGE;
    Item.row = nRow;
    Item.col = nCol;
    if (!GetItem(&Item)) return FALSE;

    int nSavedDC = pDC->SaveDC();

    pDC->SetBkMode(TRANSPARENT);

	CGridCell* pCell = GetCell(nRow, nCol);

    if (Item.state & GVIS_FOCUSED && !pDC->IsPrinting()) 
	{
        rect.right++; rect.bottom++;    // FillSolidRect doesn't draw RHS or bottom
        if (bEraseBk) pDC->FillSolidRect(rect, GetTextBkColor());
        rect.right--; rect.bottom--;    
        pDC->SelectStockObject(BLACK_PEN);
        pDC->SelectStockObject(NULL_BRUSH);
        pDC->Rectangle(rect);
        pDC->SetTextColor(StatusColor(pCell->m_status));//GetTextColor());

        rect.DeflateRect(1,1);

    }
	else if (Item.state & GVIS_READONLY)
	{
		rect.right++; rect.bottom++;    // FillSolidRect doesn't draw RHS or bottom
        pDC->FillSolidRect(rect, ::GetSysColor(COLOR_BTNFACE));
        rect.right--; rect.bottom--;
        pDC->SetTextColor(::GetSysColor(COLOR_GRAYTEXT));
	}
	else if (Item.state & GVIS_DISABLED)
	{
		rect.right++; rect.bottom++;    // FillSolidRect doesn't draw RHS or bottom
        pDC->FillSolidRect(rect, ::GetSysColor(COLOR_BTNFACE));
        rect.right--; rect.bottom--;
        pDC->SetTextColor(::GetSysColor(COLOR_GRAYTEXT));
	}



	else if (Item.state & GVIS_SELECTED && !pDC->IsPrinting()) 
	{
         rect.right++; rect.bottom++;    // FillSolidRect doesn't draw RHS or bottom
        pDC->FillSolidRect(rect, ::GetSysColor(COLOR_HIGHLIGHT));
        rect.right--; rect.bottom--;
        pDC->SetTextColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
    }
	else {
        rect.right++; rect.bottom++;    // FillSolidRect doesn't draw RHS or bottom
        if (bEraseBk){
			// i added
			if (!pCell->IsSelectable())
				pDC->FillSolidRect(rect, RGB(255,255,255));
			else
				pDC->FillSolidRect(rect, GetTextBkColor());
		}
        rect.right--; rect.bottom--;
        pDC->SetTextColor(StatusColor(pCell->m_status));//GetTextColor());
    }

    if (Item.state & GVIS_DROPHILITED && !pDC->IsPrinting()) {
        pDC->SelectStockObject(BLACK_PEN);
        pDC->SelectStockObject(NULL_BRUSH);
        pDC->Rectangle(rect);
    }

	if (!pDC->IsPrinting())//printing uses the printer font
		pDC->SelectObject(&m_Font);

    rect.DeflateRect(m_nMargin, 0);

    if (m_pImageList && Item.iImage >= 0) {
        IMAGEINFO Info;
        if (m_pImageList->GetImageInfo(Item.iImage, &Info)) {
            int nImageWidth = Info.rcImage.right-Info.rcImage.left+1;
			//we want a white icon when the grid is highlighted.
			if (pDC->IsPrinting())//need special function to print bitmaps
				DrawImage(pDC, rect.TopLeft(), Item.iImage);
			else if (Item.state & GVIS_SELECTED && !(Item.state & GVIS_FOCUSED) && !pDC->IsPrinting()) 
				m_pImageList->Draw(pDC, Item.iImage+(3*3), rect.TopLeft(), ILD_TRANSPARENT);
			else
				m_pImageList->Draw(pDC, Item.iImage+(3*pCell->m_status), rect.TopLeft(), ILD_TRANSPARENT);
            rect.left += nImageWidth+m_nMargin;
        }
    }
    InsertRichText(pDC, Item.szText, -1, rect, Item.nFormat);

    pDC->RestoreDC(nSavedDC);
    return TRUE;
}

void CTableGrid::Print(CDC* pDC, CPrintInfo* pInfo)
{
	// Create the printer font
	CFont font;
	font.CreatePointFont(80, "Arial");
	//and make it bold
	LOGFONT lf;
    VERIFY(font.GetLogFont(&lf));
    lf.lfWeight = FW_BOLD;
    VERIFY(m_PrinterFont.CreateFontIndirect(&lf));

	CFont *pOldFont = pDC->SelectObject(&m_PrinterFont);

    // Print the column headings
	pInfo->m_rectDraw.top +=  5;
    pInfo->m_rectDraw.bottom = pInfo->m_rectDraw.top + GetFixedRowHeight(); 
    PrintColumnHeadings(pDC, pInfo);

    // We need to find out which row to start printing for this page.
    int nTotalRowHeight = 0;
    UINT nNumPages = 1;
    int nCurrPrintRow = GetFixedRowCount();

    while (nCurrPrintRow < GetRowCount() && nNumPages < pInfo->m_nCurPage)
    {
        nTotalRowHeight += GetRowHeight(nCurrPrintRow);
        if (nTotalRowHeight > m_nPageHeight) {
            nNumPages++;
	        if (nNumPages == pInfo->m_nCurPage) break;
            nTotalRowHeight = GetRowHeight(nCurrPrintRow);
        }
        nCurrPrintRow++;
    }
    if (nCurrPrintRow >= GetRowCount()) return;

    // Draw as many rows as will fit on the printed page.
    // Clip the printed page so that there is no partially shown
    // row at the bottom of the page (the same row which will be fully
    // shown at the top of the next page).

    BOOL bFirstPrintedRow = TRUE;
    CRect rect = CRect(pInfo->m_rectDraw);
    rect.bottom += -1;
    while (nCurrPrintRow < GetRowCount())
    {
        rect.top = rect.bottom+1;
        rect.bottom = rect.top + GetRowHeight(nCurrPrintRow) - 1;

   //     if (rect.bottom > m_nPageHeight) break;            // Gone past end of page

        rect.right = pInfo->m_rectDraw.left - 1;
        for (int col = 0; col < GetColumnCount(); col++)
        {
            rect.left = rect.right+1;
			int nWidth = GetColumnWidth(col);
            rect.right = rect.left + GetColumnWidth(col) - 1;

            DrawCell(pDC, nCurrPrintRow, col, rect);

            if (m_nGridLines == GVL_BOTH || m_nGridLines == GVL_HORZ) 
            {
                int Overlap = (col == 0)? 0:1;
                pDC->MoveTo(rect.left-Overlap, rect.bottom);
                pDC->LineTo(rect.right, rect.bottom);
                if (nCurrPrintRow == 0) {
                    pDC->MoveTo(rect.left-Overlap, rect.top);
                    pDC->LineTo(rect.right, rect.top);
                }
            }
            if (m_nGridLines == GVL_BOTH || m_nGridLines == GVL_VERT) 
            {
                int Overlap = (bFirstPrintedRow)? 0:1;
                pDC->MoveTo(rect.right, rect.top-Overlap);
                pDC->LineTo(rect.right, rect.bottom);    
                if (col == 0) {
                    pDC->MoveTo(rect.left, rect.top-Overlap);
                    pDC->LineTo(rect.left, rect.bottom);    
                }
            }

        }
        nCurrPrintRow++;
        bFirstPrintedRow = FALSE;
    }
	pInfo->m_rectDraw.bottom = rect.bottom + 5;
	pDC->SelectObject(pOldFont);
	m_PrinterFont.DeleteObject();

}


void CTableGrid::InsertRichText(CDC* pDC, LPCTSTR lpString, int nCount, LPRECT rect, UINT uFormat)
{
	CString str = lpString;
	CRect calcRect;
	CRect drawRect = CRect(rect);

	while (str.Find("$") != -1)
	{
		int grkPos = str.Find('$');		//find greek letter
		int strlen = str.GetLength();
		if (grkPos >= (strlen - 1) )
			break;
		CFont* pFont = new CFont();
		pFont->CreatePointFont(100, "Symbol", pDC);

		// insert text before greek letter
		CString text = str.Left(grkPos);
		if (!text.IsEmpty()){
			DrawText(pDC->m_hDC, text, -1, calcRect, DT_CALCRECT);
			drawRect.right = drawRect.left + calcRect.Width();
			DrawText(pDC->m_hDC, text, -1, drawRect, uFormat);
			drawRect.left = drawRect.right;
		}

		CString c = str[grkPos +1]; //make sure letter follows $
		if ( ((c >= "a") && ( c<="z"))||((c >= "A") && ( c<="Z")) )
		{
			CFont* pOldFont = pDC->SelectObject(pFont);
			DrawText(pDC->m_hDC, c, -1, calcRect, DT_CALCRECT);
			drawRect.right = drawRect.left + calcRect.Width();
			DrawText(pDC->m_hDC, c, -1, drawRect, uFormat);
			pDC->SelectObject(pOldFont);
			drawRect.left = drawRect.right;

		}
		else
		{
			CString tempStr = "$" + c; //insert actual $ and what follows
			DrawText(pDC->m_hDC, tempStr, -1, calcRect, DT_CALCRECT);
			drawRect.right = drawRect.left + calcRect.Width();
			DrawText(pDC->m_hDC, tempStr, -1, drawRect, uFormat);
			drawRect.left = drawRect.right;

		}
		str = str.Mid(grkPos+2);
		delete pFont;
		
	}
	// replace any text left over??change in rect??
	if (!str.IsEmpty()){
		drawRect.right = rect->right;
		DrawText(pDC->m_hDC, str, -1, drawRect, uFormat);
	}
}

char CTableGrid::GetChar()
{
	return 't';//purely virtual

}

void CTableGrid::Enable(BOOL bEnable)
{
	Deselect();
	for (int col=0; col<GetColumnCount(); col++)
	{
		for (int row=0; row<GetRowCount(); row++)
		{
			UINT state = GetItemState(row, col);
			
			if (bEnable)
				SetItemState(row, col, (state & ~GVIS_DISABLED));
			else
				SetItemState(row, col, (state | GVIS_DISABLED));
		}
		RedrawColumn(col);
	}
	EnableWindow(bEnable);

}

void CTableGrid::CheckObject()
{
	//might want to add fucntion here that goes through the table and checks every cell
	///

}
////////////////////////////////////////////////////////////////////////////////////////
//CMenuGrid
////////////////////////////////////////////////////////////////////////////////////////

IMPLEMENT_DYNCREATE(CMenuGrid, CGridCtrl);

CMenuGrid::CMenuGrid()
{
	m_bPressed = FALSE;
	
}

BOOL CMenuGrid::Create(CWnd* pParentWnd, int ctrlID, CPoint pos)
{
	m_position = CRect(CPoint(pos.x, pos.y), CSize(20, CTRL_HEIGHT));
	m_nId = ctrlID;

	BOOL bResult =  CGridCtrl::Create(m_position, pParentWnd, m_nId);
	DWORD dwStyle = WS_CHILD | WS_TABSTOP | WS_VISIBLE;
	SetWindowLong(GetSafeHwnd(), GWL_STYLE, dwStyle);//no border on menu grid
	InsertRow(m_strText);
	SetColumnType(0, GVET_LISTBOX);
	SetGridLines(GVL_NONE);
	SetColumnCount(1);
	AutoSizeColumn(0);
	SetFixedTextColor(::GetSysColor(COLOR_GRAYTEXT));
	MoveControl(m_position);

	return bResult;
	
}


CMenuGrid::~CMenuGrid()
{
}


BEGIN_MESSAGE_MAP(CMenuGrid, CGridCtrl)
	//{{AFX_MSG_MAP(CMenuGrid)
	//}}AFX_MSG_MAP
    ON_WM_LBUTTONUP()
    ON_WM_LBUTTONDOWN()

END_MESSAGE_MAP()
///////////////////////////////////////////////////////////////////////////////////////////////
//
void CMenuGrid::FillListItems(int nCol, LPARAM cltList)
{
	CComboBox* pCombo;
	CListBox* pList;
	if (GetColumnType(nCol) == GVET_COMBOBOX)
		pCombo = (CComboBox*)cltList;
	else
		pList = (CListBox*)cltList;

	if (GetItemType() == ITEM_CHKSOLV){
		if (nCol == 0)
		{
			for (int i=0; i<6; i++){
				CString str;
				str.Format("%d", i);
				pList->AddString(str);
			}
		}
	}

}

void CMenuGrid::Enable(BOOL bEnable)
{
	SetFixedColumnCount(!bEnable);
	EnableWindow(bEnable);
	//Dilemma
//	if (bEnable)
//		m_pItem->SetFocus();

}

BOOL CMenuGrid::IsDisabled()
{
	if (GetFixedColumnCount()>0)
		return TRUE;
	return FALSE;
}

BOOL CMenuGrid::IsPressed()
{
	return  m_bPressed;
}

BOOL CMenuGrid::IsFocused()
{
	   GV_ITEM Item;
    Item.mask = GVIF_TEXT | GVIF_FORMAT | GVIF_STATE | GVIF_IMAGE;
    Item.row = 0;
    Item.col = 0;
    if (!GetItem(&Item)) return FALSE;

	return (Item.state & GVIS_FOCUSED);
}

char CMenuGrid::GetChar()
{
	return 'g';//purely virtual

}

CString CMenuGrid::GetHelpString()
{
	CString strHelp;
	ASSERT(GetItemType() == ITEM_CHKSOLV);
	int nCtrl = (GetCtrlID() - 15000)/10;
	//Get the control number from the button id
	if (nCtrl > 2 )
		strHelp = "unknowns";
	else
		strHelp = "equations";

	return strHelp;

}


void CMenuGrid::OnDraw(CDC* pDC)
{
	
    GV_ITEM Item;
    Item.mask = GVIF_TEXT | GVIF_FORMAT | GVIF_STATE | GVIF_IMAGE;
    Item.row = 0;
    Item.col = 0;
    if (!GetItem(&Item)) return;


	CRect rect;

	CRect clipRect;
	if (pDC->GetClipBox(&clipRect) == ERROR) return;

	EraseBkgnd(pDC);            // OnEraseBkgnd does nothing, so erase bkgnd here.
                                // This necessary since we may be using a Memory DC.

	int nSavedDC = pDC->SaveDC();


	CRect clRect;
	GetClientRect(&clRect);

	CFont* pOldFont = pDC->SelectObject(&m_Font);


	CPen penBtnHiLight(PS_SOLID, 0, GetSysColor(COLOR_BTNHILIGHT)); 
    CPen pen3DLight(PS_SOLID, 0, GetSysColor(COLOR_3DLIGHT));       
    CPen penBtnShadow(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW));   
    CPen pen3DDKShadow(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)); 

	COLORREF crText = GetTextColor();

	if (IsDisabled())
	{
		CBrush br(::GetSysColor(COLOR_BTNFACE));
		pDC->FillRect(&clRect, &br);
		pDC->SetBkColor(::GetSysColor(COLOR_BTNFACE));
		crText = GetSysColor(COLOR_GRAYTEXT);

	}
	else
		crText = StatusColor(m_status);

	CPen* pOldPen = pDC->SelectObject(&penBtnHiLight);
	pDC->MoveTo(clRect.left, clRect.bottom-1);
	pDC->LineTo(clRect.left, clRect.top);
	pDC->LineTo(clRect.right, clRect.top);

	pDC->SelectObject(pen3DLight);
	pDC->MoveTo(clRect.left+1, clRect.bottom-1);
	pDC->LineTo(clRect.left+1, clRect.top+1);
	pDC->LineTo(clRect.right, clRect.top+1);
	
	pDC->SelectObject(pen3DDKShadow);
	pDC->MoveTo(clRect.left, clRect.bottom-1);
	pDC->LineTo(clRect.right-1, clRect.bottom-1);
	pDC->LineTo(clRect.right-1, clRect.top-1);

	pDC->SelectObject(penBtnShadow);
	pDC->MoveTo(clRect.left+1, clRect.bottom-2);
	pDC->LineTo(clRect.right-2, clRect.bottom-2);
	pDC->LineTo(clRect.right-2, clRect.top);

	if (IsPressed())
	{
		CBrush brBtnShadow(GetSysColor(COLOR_BTNSHADOW));
		pDC->FrameRect(&clRect, &brBtnShadow);

	}

	if (IsFocused())
	{
		pDC->SelectStockObject(BLACK_PEN);
		CRect focusRect;
		focusRect.SetRect((clRect.left+3), (clRect.top+2), (clRect.right-3), (clRect.bottom-3) );
		pDC->DrawFocusRect(focusRect);
	}


	pDC->SetTextColor(crText);
	pDC->DrawText(m_strText, clRect, DT_CENTER |DT_VCENTER | DT_SINGLELINE);
	pDC->SelectObject(pOldFont);
	pDC->SelectObject(pOldPen);

	pDC->RestoreDC(nSavedDC);


#ifdef USE_MEMDC                        // Use a memDC for flicker free update
}
#else                                   // Use normal DC - this helps in debugging
}
#endif

void CMenuGrid::Select(BOOL bSelect)
{
	m_bPressed = bSelect;
//	((CHiLevelVw*)GetParent())->m_pFocusedItem = m_pItem;
	Invalidate();

}

void CMenuGrid::SetFocus(BOOL bFocus)
{
      int nState = GetItemState(0, 0);

	  if (bFocus)
		  SetItemState(0, 0, GVIS_FOCUSED);
	  else
		  SetItemState(0, 0, ~GVIS_FOCUSED);
	  Invalidate();
}



void CMenuGrid::OnEndEditCell(int nRow, int nCol, CString str)
{
    SetItemText(nRow, nCol, str);

	m_strText = str;
	////
	AutoSizeColumns();
	if (m_bInitState)
	{
		m_bInitState = FALSE;
		for (int index = 0; index < nItems; index++){
			if ( (items[index].id == m_pItem->m_nItemType)
				&&(items[index].ctrlType == 'g') )
			{
				if (items[index].str == str)
				{
					m_bInitState = TRUE;
					break;
				}
			}
		}
	}

	CheckObject();
	m_pItem->UpdateCtrlsthatFollow(this);
	COutlineItem* pNextItem = m_pItem->GetStage()->GetNext(GetItemType());
	if (pNextItem != NULL)
		pNextItem->Enable(TRUE);
	m_pItem->GetStage()->m_pView->GetDocument()->SetModifiedFlag();

}


void CMenuGrid::OnLButtonDown(UINT nFlags, CPoint point) 
{
	Select(TRUE);
	SetCapture();
	LogEventf(EV_LBUTTONDOWN_GRID,"%d %d %d", m_nId, point.x, point.y);	


    HWND hOldFocusWnd = ::GetFocus();

    m_LeftClickDownPoint = point;
    m_LeftClickDownCell = GetCellFromPt(point);
    if (!IsValid(m_LeftClickDownCell)) return;

	//i added
	CGridCell* pCell = GetCell(m_LeftClickDownCell.row, m_LeftClickDownCell.col);
	if (!pCell->IsSelectable()) return;


    m_SelectionStartCell = (nFlags & MK_SHIFT)? m_idCurrentCell : m_LeftClickDownCell;

	CWnd::SetFocus();        // Auto-destroy any InPlaceEdit's

    SetFocusCell(-1,-1);
    SetFocusCell(max(m_LeftClickDownCell.row, m_nFixedRows),
                     max(m_LeftClickDownCell.col, m_nFixedCols));


    // If Ctrl pressed, save the current cell selection. This will get added
    // to the new cell selection at the end of the cell selection process
    m_PrevSelectedCellMap.RemoveAll();
    if (nFlags & MK_CONTROL) {
        for (POSITION pos = m_SelectedCellMap.GetStartPosition(); pos != NULL; )
        {
            DWORD key;
            CCellID cell;
            m_SelectedCellMap.GetNextAssoc(pos, key, (CCellID&)cell);
            m_PrevSelectedCellMap.SetAt(key, cell);
        }
    }
    
    if (m_LeftClickDownCell.row < GetFixedRowCount())
		OnFixedRowClick(m_LeftClickDownCell);
    else if (m_LeftClickDownCell.col < GetFixedColumnCount())
        OnFixedColumnClick(m_LeftClickDownCell);
    else
    {
        m_MouseMode = m_bListMode? MOUSE_SELECT_ROW : MOUSE_SELECT_CELLS;
        OnSelecting(m_LeftClickDownCell);
    }

    m_LastMousePoint = point;
	m_MouseMode = MOUSE_PREPARE_EDIT;
	return;

}

void CMenuGrid::OnLButtonUp(UINT nFlags, CPoint point) 
{
	Select(FALSE);
	ReleaseCapture();
	m_pItem->SetFocus();
	if (m_pItem->m_nItemType == ITEM_CHKSOLV){
		for (int i=0; i<m_pItem->m_controls.GetSize(); i++){
			CItemCtrl* pCtrlInList = m_pItem->m_controls[i];
			pCtrlInList->SetFocus(this==pCtrlInList);//unset all but 1
		}
	}

	CGridCtrl::OnLButtonUp(nFlags, point);
}

/////////////////////////////////////////////////////////////////////////////
// CCtrlData
//////////////////////////////////////////////////////////////////////////
// 
// Holds control data so controls can be created at runtime
//////////////////////////////////////////////////////////////////////////
IMPLEMENT_SERIAL(CCtrlData, CObject, VERSIONABLE_SCHEMA | 2);

CCtrlData::CCtrlData()
{

}


CCtrlData::~CCtrlData()
{//if we are previewing, the control which handles the deletion of its properties is never
//created.  Therefore, the property list must be deleted here. How shall I 
//handle problems without plans?  Maybe should make sure doesn't save plan in author view
//Or if plan not included, just delete the property list here.
	ASSERT_VALID(m_pDocument);	// back pointer better be set correctly
	if (m_pDocument->m_bFilePreview/* ||  !m_pDocument->m_bIncludePlan*/){
	     // Empty data list
	     while (!m_properties.IsEmpty()){
		     CCheckedObj* pProp = m_properties.RemoveHead();
		     delete pProp;
		 }
	}

}
   
void CCtrlData::Serialize(CArchive& ar)
{
	UINT nClassVersion = ar.IsLoading() ? ar.GetObjectSchema() : 0;
	UINT nDocVersion = ((CFBDDoc*) ar.m_pDocument)->m_nVersion;

	CObject::Serialize(ar);
  	if (ar.IsStoring())
   	{//started with version 1
		 ar << m_chType;
		 ar << m_strLabel;
//		 ar << m_bEnabled;//removed in version 2
		 ar << m_bInitState;
		 ar << m_nTableId;
	} else {
		m_pDocument = (CFBDDoc*) ar.m_pDocument;
    	ASSERT_VALID(m_pDocument);
    	ASSERT_KINDOF(CFBDDoc, m_pDocument);
		 DWORD dwTemp;
		 ar >> m_chType;
		 ar >> m_strLabel;
		 if (nClassVersion < 2)
			 ar >> dwTemp;
		 ar >> m_bInitState;
		 ar >> m_nTableId;

	}
	m_properties.Serialize(ar);

}

////////////////////////////////////////////////////////////////////////////
// CCtrlData message handlers

////////////////////////////////////////////////////////////////////////////
void CCtrlData::Update(CItemCtrl* pCtrl)
{
	m_chType = pCtrl->GetChar();
	m_strLabel = pCtrl->GetText();
//	m_bEnabled = pCtrl->IsWindowEnabled();
	m_bInitState = pCtrl->m_bInitState;
	if (pCtrl->IsKindOf(RUNTIME_CLASS(CTableGrid)))
	{
		CTableGrid* pTable = (CTableGrid*)pCtrl;

		m_nTableId = pTable->GetId();
		POSITION pos = pTable->m_properties.GetHeadPosition();
		while (pos != NULL){
			//find the matching table row
			CTableRow* pProp = 	pTable->m_properties.GetNext(pos);
			int nCount = pTable->GetRowCount();
			for (int nRow = 0; nRow<pTable->GetRowCount(); nRow++){
				CGridCell* pCell = pTable->GetCell(nRow, 0);
				if (pCell != NULL){
					CTableRow* pRow = (CTableRow*)pCell->lParam;
					if (pRow == pProp)
						break;
				}
			}
			for (int nCol=1; nCol<pTable->GetColumnCount(); nCol++)
			{
				int nImage = pTable->GetItemImage(nRow, nCol);
				CString str = pTable->GetItemText(nRow, nCol);
				pProp->m_strings.AddTail(str);
				pProp->m_images.AddTail(nImage);
			}


			m_properties.AddTail(pProp);
		}

	}
	if (pCtrl->IsKindOf(RUNTIME_CLASS(CCellCtrl)))
	{
		CCellCtrl* pCell = (CCellCtrl*)pCtrl;
		POSITION pos = pCell->m_properties.GetHeadPosition();
		while (pos != NULL){
			CVariable* pProp = (CVariable*)pCell->m_properties.GetNext(pos);
			m_properties.AddTail(pProp);
		}
	}

}

void CMenuGrid::CheckObject()
{
	LPCTSTR pszResult;
	CString stageId;
	stageId.Format("stage-%d", GetStageID());

	if (m_bInitState)
		return;

	if ( GetItemType() == ITEM_CHKSOLV){
		CString str = GetHelpString();
		CString strLispFunction;
		strLispFunction.Format("count-plan-stage-%s", str);
		pszResult =
			HelpSystemExecf( "(%s %s %s)",
				strLispFunction,
				STR2ARG(m_strText),
				STR2ARG(stageId)
				);

	}


	ApplyStatus(pszResult);
	return;
}

CTableRow* CTableGrid::GetMatchingProp(CString strDef)
{
	POSITION pos = m_properties.GetHeadPosition();
	while (pos != NULL)
	{
		CTableRow* pExProp = m_properties.GetNext(pos);
		if (_stricmp(strDef, pExProp->m_strDef) == 0)//case insensitive
			return pExProp;
	}
	return NULL;
}
