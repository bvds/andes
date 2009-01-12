// ItemCtrl.cpp : implementation file
//

#include "stdafx.h"
#include "fbd.h"
#include "FBDDoc.h"//Brings in stageobj
#include "Helpifc.h"
#include "FBDObj.h" 
#include "MainFrm.h"
#include "VariableDlg.h"
#include "SysDlg.h"
#include "PropertyDlg.h"
#include "FormulaDlg.h"
#include "HiLevelVw.h"



#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CItemCtrl
IMPLEMENT_DYNCREATE(CItemCtrl, CWnd);


CItemCtrl::CItemCtrl()
{
	m_bInitState = TRUE;
	m_nMargin = 0;
	m_pItem = NULL;
}

CItemCtrl::~CItemCtrl()
{
}


BEGIN_MESSAGE_MAP(CItemCtrl, CWnd)
	//{{AFX_MSG_MAP(CItemCtrl)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CItemCtrl message handlers

BOOL CItemCtrl::Create(CWnd* pParentWnd, int ctrlID, CPoint pos)
{
	return TRUE;
}

void CItemCtrl::UpdatePosition(int x, int y)
{
	CRect pos = m_position;
	pos.OffsetRect(x, y);
	SetPosition(pos);
	MoveControl(pos);

}

void CItemCtrl::MoveControl(CRect pos)
{
	CHiLevelVw* pView = (CHiLevelVw*)GetParent();
	CPoint scrollPt = pView->GetScrollPosition();
	pos.OffsetRect(-scrollPt.x, -scrollPt.y);
	MoveWindow(pos);
}



void CItemCtrl::Select(BOOL bSelect)
{
}

void CItemCtrl::Enable(BOOL bEnable)
{
	EnableWindow(bEnable);
}

void CItemCtrl::SetId(int id)
{
	m_nId = id;
	SetDlgCtrlID(m_nId);
}

void CItemCtrl::SetFocus(BOOL bFocus)
{
	return;

}


CFont* CItemCtrl::GetFont()
{
	return GetParent()->GetFont();
}

void CItemCtrl::SetPosition(CRect pos)
{
	m_position = CRect(pos);
}

void CItemCtrl::SetText(CString text)
{
	m_strText = text;
	SetWindowText(m_strText);
	CClientDC dc(NULL);
	CFont* pFont = GetFont();
	CFont* pOldFont = dc.SelectObject(pFont);
	CSize size = dc.GetTextExtent(m_strText);
    dc.SelectObject(pOldFont);
	CRect oldPos = GetPosition();
	CRect newPos = CRect(CPoint(m_position.TopLeft()), 
		CSize(size.cx + m_nMargin, m_position.Height()));
	SetPosition(newPos);
	SetWindowPos(NULL, 0, 0, size.cx + m_nMargin, m_position.Height(), SWP_NOMOVE|SWP_NOZORDER);
	m_pItem->m_position.right += (newPos.Width() - oldPos.Width());
	
}

void CItemCtrl::ShowMenu(UINT menuId)
{
	CMenu menu;
	VERIFY(menu.LoadMenu(menuId));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	CScrollView* pWnd = (CScrollView*)GetParent();
	CRect pos = GetPosition();
	CPoint scrollPt = pWnd->GetScrollPosition();
	pos.OffsetRect(-scrollPt.x, -scrollPt.y);
	CPoint pt = CPoint(pos.right, pos.top);
	pWnd->ClientToScreen(&pt);


	CWnd* pWndPopupOwner = this;

	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_LEFTBUTTON, 
		pt.x, pt.y,	pWndPopupOwner);
	
}

void CItemCtrl::SetOutlineItem(COutlineItem* pItem)
{
	m_pItem = pItem;
}

char CItemCtrl::GetChar()
{
	return 'i';//purely virtual

}

CStageObj* CItemCtrl::GetStage()
{
	return m_pItem->GetStage();
}

int CItemCtrl::GetStageID()
{
	return GetStage()->m_nId;
}

int CItemCtrl::GetCtrlID()
{
	return (m_nId - (GetStageID()*100));
}

int CItemCtrl::GetItemType()
{
	return (m_nId%10);
}


int CItemCtrl::HitTest(CPoint point)
{
   	ASSERT_VALID(this);
   	

   	CRect normPos = m_position;
   	normPos.NormalizeRect();
   	if (point.x >= normPos.left && point.x <= normPos.right &&
   		point.y >= normPos.top && point.y <= normPos.bottom)
   		return 1;
   	return 0;
}
/////////////////////////////////////////////////////////////////////////////


#define CTRL_HEIGHT	20


/////////////////////////////////////////////////////////////////////////////
// CItemBtn
IMPLEMENT_DYNCREATE(CItemBtn, CItemCtrl);

CItemBtn::CItemBtn()
{
	m_nMargin = 16;
}

CItemBtn::~CItemBtn()
{
}


BEGIN_MESSAGE_MAP(CItemBtn, CItemCtrl)
	//{{AFX_MSG_MAP(CItemBtn)
	ON_CONTROL_REFLECT_EX(BN_CLICKED, OnClicked)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CItemBtn message handlers
///////////////////////////////////////////////////////////////////////

BOOL CItemBtn::Create(CWnd* pParentWnd, int ctrlID, CPoint pos)
{
	CClientDC dc(NULL);
	CFont* pFont = pParentWnd->GetFont();
	CFont* pOldFont = dc.SelectObject(pFont);
	CSize size = dc.GetTextExtent(m_strText);
    dc.SelectObject(pOldFont);
//	int ctrlWidth = GetWidth(ctrlID);
	m_position = CRect(CPoint(pos.x, pos.y), CSize(size.cx, CTRL_HEIGHT));
	m_position.InflateRect(0, 0, m_nMargin, 0);
	m_nId = ctrlID;
	// Child window Id is offset from base by number created so far
	CWnd* pWnd = this;
	BOOL bResult = pWnd->Create(_T("BUTTON"), m_strText, 
		BS_PUSHBUTTON | WS_CHILD |WS_VISIBLE, 
		m_position, pParentWnd, ctrlID);
	SetFont(pFont);
	MoveControl(m_position);

	return bResult;
	

}

BOOL CItemBtn::OnClicked() 
{
	//for logging purposes, can't be a log button cause created at runtime
	CString strText;
	GetWindowText(strText);
	// No need to use control name, button text will identify for human reader
	LogEventf(EV_PLANBTN_CLICK, "%d %s", GetDlgCtrlID() , (LPCTSTR) strText);

	//Set the focused item
	m_pItem->SetFocus();

	//need to remove stage part from id
	if (GetCtrlID() == IDB_BTN_ADDPROP)
	{						
		//Show menu of possible properties to add
		if (!LogPlayerInPlayback()) 
			ShowMenu(IDR_POPUP_PROPERTY);
	}
	else if (GetCtrlID() == IDB_BTN_DELPROP)
	{
		GetStage()->OnPropertyDelete();//sets modified flag
		CDocument* pDoc = theApp.GetDocument();
		if (pDoc != NULL)
		{
			pDoc->UpdateAllViews(NULL, HINT_DELETE_PROP, GetStage());
			pDoc->UpdateAllViews(NULL, HINT_UPDATE_HILEVELVW, NULL);
		}
	}
	else if (GetCtrlID() == IDB_BTN_SHOWEQN)
	{
		CString strStgID;
		strStgID.Format("stage-%d", GetStageID());

		CString pszResult = HelpSystemExecf("(show-plan-stage-equations %s) ", 
									STR2ARG(strStgID));
		
		theApp.GetMainFrame()->ShowHint(pszResult);

	}

	return TRUE;//do not let parent handle

}

char CItemBtn::GetChar()
{
	return 'b';

}

/////////////////////////////////////////////////////////////////////////////
// CItemStc
IMPLEMENT_DYNCREATE(CItemStc, CItemCtrl);


CItemStc::CItemStc()
{
}

CItemStc::~CItemStc()
{
}


BEGIN_MESSAGE_MAP(CItemStc, CItemCtrl)
	//{{AFX_MSG_MAP(CItemStc)
	ON_WM_ERASEBKGND()
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CItemStc message handlers
BOOL CItemStc::Create(CWnd* pParentWnd, int ctrlID, CPoint pos)
{
	CClientDC dc(NULL);
	CFont* pFont = ((CHiLevelVw*)pParentWnd)->GetBoldFont();
	CFont* pOldFont = dc.SelectObject(pFont);
	CSize size = dc.GetTextExtent(m_strText);
	//little hard code cheat here, to line up the cells
	//for the system, motion and use law
	//The cells will line up if their statics are all the same size
	int itemId = ctrlID % 10;//Get the item id from the button id
//	if ((itemId > ITEM_SOUGHT)  && (itemId < ITEM_SYSPROP))
//		size.cx = 55;

    dc.SelectObject(pOldFont);
	m_position = CRect(CPoint(pos.x, pos.y), CSize(size.cx, CTRL_HEIGHT));
	m_nId = ctrlID;
	CWnd* pWnd = this;
	BOOL bResult = pWnd->Create(_T("STATIC"), m_strText, 
		SS_CENTERIMAGE | WS_CHILD |WS_VISIBLE,
		m_position, pParentWnd, ctrlID);
	SetFont(pFont);
	MoveControl(m_position);
	return bResult;

}

void CItemStc::Enable(BOOL bEnable)
{
	//Eat this in static controls
	//statics are initially enabled and remain so.
}

char CItemStc::GetChar()
{
	return 's';//purely virtual

}

CFont* CItemStc::GetFont()
{
	return ((CHiLevelVw*)GetParent())->GetBoldFont();
}

/////////////////////////////////////////////////////////////////////////////
// CCheckedItem
IMPLEMENT_DYNCREATE(CCheckedItem, CItemCtrl);


CCheckedItem::CCheckedItem()
{
	m_status = statusUnknown;
}

   
void CCheckedItem::ApplyStatus(LPCTSTR pszResult)
{ 
	Status oldStatus = m_status;
	// Translate Lisp result into new status
	// Note piggybacked command might execute here!
	CStringList strList;
 	CCheckedObj::ApplyStatus(pszResult, m_status, strList);
	
 	// redraw if status changed.
 	if (m_status != oldStatus) 
 		Invalidate(); 
}

// Get color to represent the given status.
COLORREF CCheckedItem::StatusColor(Status status)
{
   
   	switch (status) {
   	case statusError:
   		return (RGB(194, 0, 0));	// red
   
   	case statusCorrect:
   		return (RGB(0, 128, 0));	// betw dark green (128) and green (255)
   
   	default:
   		// This case happened for serialized objs since status not saved.
   		// Now fixed in deserialization, so shouldn't ever happen
   		TRACE("Status Color: Bad status value!\n"); 
   		m_status = statusUnknown;
   		// fall through...
   	case statusUnknown:
   		return (RGB(0, 0, 0));		// black
   	}
}


/////////////////////////////////////////////////////////////////////////////
// CCellCtrl
IMPLEMENT_DYNCREATE(CCellCtrl, CCheckedItem);


CCellCtrl::CCellCtrl()
{
	state = NULL;
	m_nMargin = 8;
//	m_MouseOnBtn = FALSE;
}

BOOL CCellCtrl::Create(CWnd* pParentWnd, int ctrlID, CPoint pos)
{
	CClientDC dc(NULL);
	CFont* pFont = pParentWnd->GetFont();
	CFont* pOldFont = dc.SelectObject(pFont);
	CSize size = dc.GetTextExtent(m_strText);
    dc.SelectObject(pOldFont);

	m_position = CRect(CPoint(pos.x, pos.y), CSize(size.cx + m_nMargin, CTRL_HEIGHT));
	m_nId = ctrlID;
	LPCTSTR classname = AfxRegisterWndClass( CS_PARENTDC | CS_DBLCLKS , AfxGetApp()->LoadStandardCursor(IDC_ARROW) );

	CWnd* pWnd = this;
	BOOL bResult = pWnd->CreateEx(WS_EX_TOOLWINDOW, classname,
		NULL, WS_CHILD |WS_VISIBLE , m_position, pParentWnd, ctrlID);
	
	SetFont(pFont);
	MoveControl(m_position);

	return bResult;

}

CCellCtrl::~CCellCtrl()
{
	// Empty data list... should only have one
	while (!m_properties.IsEmpty()){
		CCheckedObj* pProp = m_properties.RemoveHead();
		delete pProp;
	}

	DestroyWindow();
}


BEGIN_MESSAGE_MAP(CCellCtrl, CItemCtrl)
	//{{AFX_MSG_MAP(CCellCtrl)
	ON_WM_PAINT()
	ON_WM_LBUTTONDOWN()
	ON_WM_ERASEBKGND()
	ON_WM_LBUTTONUP()
	ON_WM_RBUTTONDOWN()
	//}}AFX_MSG_MAP
	ON_CONTROL_REFLECT_EX(BN_CLICKED, OnClicked)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCellCtrl message handlers
void CCellCtrl::OnPaint() 
{
	CPaintDC dc(this); // device context for painting
	CFont* pOldFont = dc.SelectObject(GetParent()->GetFont());
	COLORREF crText;

	
	CRect clRect;
	GetClientRect(&clRect);

	CPen penBtnHiLight(PS_SOLID, 0, GetSysColor(COLOR_BTNHILIGHT)); 
    CPen pen3DLight(PS_SOLID, 0, GetSysColor(COLOR_3DLIGHT));       
    CPen penBtnShadow(PS_SOLID, 0, GetSysColor(COLOR_BTNSHADOW));   
    CPen pen3DDKShadow(PS_SOLID, 0, GetSysColor(COLOR_3DDKSHADOW)); 


	if (IsDisabled())
	{
		CBrush br(::GetSysColor(COLOR_BTNFACE));
		dc.FillRect(&clRect, &br);
		dc.SetBkColor(::GetSysColor(COLOR_BTNFACE));
		crText = GetSysColor(COLOR_GRAYTEXT);

	}
	else
		crText = StatusColor(m_status);

	CPen* pOldPen = dc.SelectObject(&penBtnHiLight);

	dc.MoveTo(clRect.left, clRect.bottom-1);
	dc.LineTo(clRect.left, clRect.top);
	dc.LineTo(clRect.right, clRect.top);

	dc.SelectObject(pen3DLight);
	dc.MoveTo(clRect.left+1, clRect.bottom-1);
	dc.LineTo(clRect.left+1, clRect.top+1);
	dc.LineTo(clRect.right, clRect.top+1);
	
	dc.SelectObject(pen3DDKShadow);
	dc.MoveTo(clRect.left, clRect.bottom-1);
	dc.LineTo(clRect.right-1, clRect.bottom-1);
	dc.LineTo(clRect.right-1, clRect.top-1);

	dc.SelectObject(penBtnShadow);
	dc.MoveTo(clRect.left+1, clRect.bottom-2);
	dc.LineTo(clRect.right-2, clRect.bottom-2);
	dc.LineTo(clRect.right-2, clRect.top);


	if (IsPressed())
	{
		CBrush brBtnShadow(GetSysColor(COLOR_BTNSHADOW));
		dc.FrameRect(&clRect, &brBtnShadow);

	}
	if (IsFocused())
	{
		dc.SelectStockObject(BLACK_PEN);
		CRect focusRect;
		focusRect.SetRect((clRect.left+3), (clRect.top+2), (clRect.right-3), (clRect.bottom-3) );
		dc.DrawFocusRect(focusRect);
	}


	dc.SetTextColor(crText);
	dc.DrawText(m_strText, clRect, DT_CENTER |DT_VCENTER | DT_SINGLELINE);
	dc.SelectObject(pOldFont);
	dc.SelectObject(pOldPen);

	// Do not call CWnd::OnPaint() for painting messages
}

void CCellCtrl::Select(BOOL bSelect)
{
	if (bSelect)
		state = state | CCIS_SELECTED;
	else
		state = state & ~CCIS_SELECTED;
	Invalidate();

}

void CCellCtrl::OnLButtonDown(UINT nFlags, CPoint point) 
{
	Select(TRUE);
	SetCapture();
	CWnd::OnLButtonDown(nFlags, point);
}

void CCellCtrl::OnLButtonUp(UINT nFlags, CPoint point) 
{
	Select(FALSE);
	ReleaseCapture();
    GetParent()->PostMessage(WM_COMMAND, MAKELONG(GetDlgCtrlID(), BN_CLICKED), 
                                         (LPARAM) GetSafeHwnd());

	CItemCtrl::OnLButtonUp(nFlags, point);
}

BOOL CCellCtrl::OnClicked()
{
	//for logging purposes, can't be a log button cause created at runtime
	// No need to use control name, button text will identify for human reader
	LogEventf(EV_PLANCELL_CLICK, "%d %s", m_nId, (LPCTSTR) m_strText);
	m_pItem->SetFocus();
	OnEditProperties(m_nId);
	return FALSE;
}

BOOL CCellCtrl::IsPressed()
{
	return (state & CCIS_SELECTED);
}

BOOL CCellCtrl::IsFocused()
{
	return (state & CCIS_FOCUSED);
}

BOOL CCellCtrl::IsDisabled()
{
	return (state & CCIS_DISABLED);
}

void CCellCtrl::SetFocus(BOOL bFocus)
{
	if (bFocus)
		state = state | CCIS_FOCUSED;
	else
		state = state & ~CCIS_FOCUSED;
	Invalidate();
}


void CCellCtrl::OnEditProperties(int id)
{
	int nId = id % 10;//get item id from cell id
	UINT menuId;

	switch (nId){

	case ITEM_DIRECTION: 
		{
		menuId = IDR_DIRECTIONS;
		if (!LogPlayerInPlayback()) 
			ShowMenu(menuId);
		}
	}
	Invalidate();
	COutlineItem* pNextItem = m_pItem->GetStage()->GetNext(nId);
	if (pNextItem != NULL)
		pNextItem->Enable(TRUE);

	m_pItem->GetStage()->m_pView->GetDocument()->SetModifiedFlag();

}




void CCellCtrl::Enable(BOOL bEnable)
{
	if (!bEnable)
		state = state | CCIS_DISABLED;
	else
		state = state & ~CCIS_DISABLED;
	Invalidate();
	EnableWindow(bEnable);
}


BOOL CCellCtrl::OnEraseBkgnd(CDC* pDC) 
{
	CRect clRect;
	GetClientRect(clRect);

	pDC->FillSolidRect(clRect, RGB(255, 255, 255));

	return TRUE;
}

void CCellCtrl::SetText(CString text)
{//differs from base class function in that we do not "SetWindowText(text)"
	m_strText = text;
	CClientDC dc(NULL);
	CFont* pFont = GetParent()->GetFont();
	CFont* pOldFont = dc.SelectObject(pFont);
	CSize size = dc.GetTextExtent(m_strText);
    dc.SelectObject(pOldFont);
	CRect oldPos = GetPosition();
	CRect newPos = CRect(CPoint(m_position.TopLeft()), 
		CSize(size.cx + m_nMargin, m_position.Height()));
	SetPosition( newPos );
	SetWindowPos(NULL, 0, 0, size.cx + m_nMargin, m_position.Height(), SWP_NOMOVE|SWP_NOZORDER);
	m_pItem->m_position.right += (newPos.Width() - oldPos.Width());


}

char CCellCtrl::GetChar()
{
	return 'c';
}

void CCellCtrl::AddProperty(CCheckedObj* pProp)
{
	m_properties.AddTail(pProp);
}

void CCellCtrl::CheckObject()
{
	LPCTSTR pszResult;
	CString stageId;
	stageId.Format("stage-%d", GetStageID());

	if (m_bInitState)
		return;

	if (GetItemType() == ITEM_DIRECTION){
		CVariable* pVec = NULL;
		CString strId;
		if (!m_properties.IsEmpty()){
			CCheckedObj* pObj = m_properties.GetHead();
			pVec = (CVariable*)pObj;
		}
		if (pVec)
			strId = pVec->m_strId;
		else if (m_strText.Find("horizontal") != -1)
			strId = "horizontal";//horizontal if both (ie two directions horz & vert)
		else
			strId = "vertical";
		pszResult = HelpSystemExecf( "(lookup-plan-stage-directions %d %s %s)",
						m_pItem->GetStage()->m_nEqs,
						STR2ARG(strId), 
					//OUR MIDDLE STRING NEED TO BE VERTICAL, HORIZONTAL OR PROPID
						STR2ARG(stageId)
						);



	}

	ApplyStatus(pszResult);

}

void CCellCtrl::OnRButtonDown(UINT nFlags, CPoint point) 
{
//	m_pItem->SetFocus();	
	CItemCtrl::OnRButtonDown(nFlags, point);
}
/////////////////////////////////////////////////////////////////////////////////
