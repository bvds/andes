// InPlaceCombo.cpp : implementation file
//
// Based on code by Motty Cohen, Chris Maunder and Jakawan  Ratiwanich
// 
//
/////////////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "FBD.h"
#include "GridCtrl.h"
#include "ExpLawDlg.h"
#include "InPlaceList.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CInPlaceList

CInPlaceList::CInPlaceList(int nRow, int nColumn, 
				 CString sInitText)
{
	m_nNumLines = 5;
	m_sInitText = sInitText;
 	m_nRow		= nRow;
 	m_nCol      = nColumn;
	m_bHasExplanations = FALSE;
	m_bHasIcons = FALSE;
	m_bCancel = FALSE;
}

CInPlaceList::~CInPlaceList()
{
	if (m_bHasExplanations)
		m_pDlg->DestroyWindow();
}

BEGIN_MESSAGE_MAP(CInPlaceList, CListBox)
	//{{AFX_MSG_MAP(CInPlaceList)
	ON_WM_NCDESTROY()
	ON_WM_MOUSEMOVE()
	ON_WM_CREATE()
	ON_CONTROL_REFLECT(LBN_SELCHANGE, OnSelchange)
	ON_WM_LBUTTONDOWN()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CInPlaceList message handlers
BOOL CInPlaceList::Create(CWnd* pParent, CRect& rect, DWORD dwStyle) 
{
	int nHeight = 16;
	rect.top = rect.bottom;
	rect.bottom = rect.top + m_nNumLines*nHeight;
	m_pParent = (CGridCtrl*)pParent;
	pParent = pParent->GetParent();
	return CListBox::Create(dwStyle, rect, pParent, IDC_IPLIST);	
}

int CInPlaceList::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CListBox::OnCreate(lpCreateStruct) == -1)
		return -1;


    SetWindowPos(&wndTopMost, 0, 0, 0, 0, SWP_NOMOVE);

	CWnd* pParent = CWnd::FromHandle(lpCreateStruct->hwndParent);
	// Add the strings, Ask them from the parent
	m_pParent->SendMessage( IPLM_FILL, m_nCol, (LPARAM)this );

	if (m_bHasExplanations){
		m_pDlg = new CExpLawDlg();
		if (!m_pDlg->Create(IDD_EXPLAIN_LAW))
			return 0;
	}
	
	
	// Get the maximum width of the text strings
	int nMaxLength = 0;
	int nTotHeight = 0;
	CClientDC dc(GetParent());
	CFont* pOldFont = dc.SelectObject(pParent->GetFont());

 	for (int i = 0; i <GetCount(); i++) {
		CString str;
   		GetText(i, str);
		nMaxLength = max(nMaxLength, dc.GetTextExtent(str).cx);
		nTotHeight = nTotHeight + 18;
	}
	
	nMaxLength += (::GetSystemMetrics(SM_CXVSCROLL) + dc.GetTextExtent(_T(" ")).cx*2);
	dc.SelectObject(pOldFont);

	CRect rect = CRect(lpCreateStruct->x, lpCreateStruct->y, 
		lpCreateStruct->x + lpCreateStruct->cx,	lpCreateStruct->y + lpCreateStruct->cy);
	
	if (rect.right < rect.left + nMaxLength + 4)
		rect.right = rect.left + nMaxLength + 4;
	if (rect.bottom < rect.top + nTotHeight)
		rect.bottom = rect.top + nTotHeight;

	m_pParent->ClientToScreen(&rect);
	pParent->ScreenToClient(&rect);
	MoveWindow(rect);
	
	SetFont(pParent->GetFont());
	SetItemHeight(0, 16);

	SetHorizontalExtent(0); // no horz scrolling

	SetFocus();
	SetCapture();
	// Set the initial text to m_sInitText
	if (SelectString(-1, m_sInitText) == LB_ERR) 
		SetWindowText(m_sInitText);		// No text selected, so restore what was there before

	return 0;
}


void CInPlaceList::EndSelect()
{
	CString		csItemText;
	DWORD		dwUserData = 0;
	int			nItem = GetCurSel();

	if (( nItem == LB_ERR ) || (m_bCancel))
	{
		csItemText = m_sInitText;
	}
	else
	{
		dwUserData = GetItemData(nItem);
		GetText( nItem, csItemText);
	}

    // Send Notification to parent (this was originally designed for listviews,
    // so we use listview structures and messages)

   // LV_DISPINFO dispinfo;

    m_dispinfo.hdr.hwndFrom = m_pParent->GetSafeHwnd();
    m_dispinfo.hdr.idFrom   = GetDlgCtrlID();
    m_dispinfo.hdr.code     = GVN_ENDLABELEDIT;
 
    m_dispinfo.item.mask       = GVIF_TEXT|GVIF_PARAM;
    m_dispinfo.item.iItem      = m_nRow;
    m_dispinfo.item.iSubItem   = m_nCol;
    m_dispinfo.item.pszText    = LPTSTR((LPCTSTR)csItemText);
    m_dispinfo.item.cchTextMax = csItemText.GetLength();
    m_dispinfo.item.lParam     = (LPARAM)dwUserData; 
	ReleaseCapture();
    DestroyWindow();
 
}



BOOL CInPlaceList::PreTranslateMessage(MSG* pMsg) 
{
	if (pMsg->message == WM_KEYDOWN){ 
		if (pMsg->wParam == VK_RETURN){
			EndSelect();
			return TRUE;
		}
		if (pMsg->wParam == VK_ESCAPE){
			m_bCancel = TRUE;
			EndSelect();
			return TRUE;
		}
	}
	return CListBox::PreTranslateMessage(pMsg);
}

void CInPlaceList::OnMouseMove(UINT nFlags, CPoint point) 
{
	//only log mouse move when change in selection or change in showing or hiding
	//of law explanation
	BOOL bOutside;
	BOOL bSelChanged = FALSE;
	int nIndex = ItemFromPoint(point, bOutside);
	CString str;
	if (nIndex >=0)
		GetText(nIndex, str);

	if (nIndex != GetCurSel() && !bOutside){
		bSelChanged = TRUE;
		SetCurSel(nIndex);
		//logging the change in selection
		LogEventf(EV_MOUSEMOVE_LIST,"%d %d %d %s", GetDlgCtrlID(), point.x, point.y, str);
	}

	if (m_bHasExplanations)
	{
		BOOL bShow = (HitTest(nIndex, point) == LBN_ARROW);
		if (bShow != m_pDlg->IsWindowVisible()){
			LogEventf(EV_MOUSEMOVE_LIST,"%d %d %d %s", GetDlgCtrlID(), point.x, point.y, str);
			if (bShow)
				LogEventf(EV_SHOW_EXP, "%s", str);//logging the showing of an explanation
			else
				LogEventf(EV_HIDE_EXP, "%s", str);//logging the hiding of an explanation

		}
		if (bShow && bSelChanged)
			LogEventf(EV_SHOW_EXP, "%s", str);//logging change in which explanation we're showing
		ShowExplanation(nIndex, bShow);//also changes explanation if selection has changed
	}

	CListBox::OnMouseMove(nFlags, point);
}

void CInPlaceList::OnSelchange() 
{
	CString strText;
	GetWindowText(strText);
	CString newText;
	GetText(GetCurSel(), newText);
	LogEventf(EV_PLANLIST_SEL, "%d %d %s %s", GetDlgCtrlID(), GetCurSel(), strText,  newText);

	EndSelect();

}


void CInPlaceList::OnNcDestroy() 
{
	CListBox::OnNcDestroy();
	delete this;
}


void CInPlaceList::ShowExplanation(int sel, BOOL bShow)
{
	if (!bShow){
		m_pDlg->ShowWindow(SW_HIDE);
		return;
	}

	m_pDlg->m_nId = sel;

	CRect rect;
	CRect oldDlgRect;
	CRect newDlgRect;
	GetWindowRect(&rect);
	int nHeight = GetItemHeight(0);
	int nTop	= (int)((sel * nHeight) + rect.top);
	m_pDlg->GetWindowRect(&oldDlgRect);
	m_pDlg->MoveWindow(rect.right, nTop, oldDlgRect.Width(), oldDlgRect.Height());
	m_pDlg->ShowWindow(SW_SHOW);
	

}

// Drawing functions
void CInPlaceList::MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct) 
{
	lpMeasureItemStruct->itemHeight = 16;
	
}

void CInPlaceList::DrawItem(LPDRAWITEMSTRUCT lpdis) 
{
	if (lpdis->itemID >= 20)
		return;

	BOOL bFocus = (lpdis->itemAction & ODA_FOCUS);
	BOOL bSelected = (lpdis->itemState & ODS_SELECTED);

	if (bSelected)
		DrawDropList(lpdis, IS_HIGHLIGHTED);
	else
		DrawDropList(lpdis, IS_NORMAL);
}

void CInPlaceList::DrawDropList(LPDRAWITEMSTRUCT lpdis, UINT nState)
{
	CDC* pDC = CDC::FromHandle(lpdis->hDC);
	CRect rcItem(lpdis->rcItem);
	CRect rcBorder;
	CRect rcBtn;

	switch (nState){
	case IS_HIGHLIGHTED:
		pDC->SetTextColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
		pDC->SetBkColor(::GetSysColor(COLOR_HIGHLIGHT));
		pDC->FillRect(&lpdis->rcItem, &CBrush(::GetSysColor(COLOR_HIGHLIGHT)) );
		if ((m_bHasIcons)&&(lpdis->itemID <3)){

			IMAGEINFO Info;
			int nImage = ((lpdis->itemID + 2)%3)+9;
			if (m_pParent->GetImageList()->GetImageInfo(nImage, &Info)) 
			{
				 int nImageWidth = Info.rcImage.right-Info.rcImage.left+1;
				 m_pParent->GetImageList()->Draw(pDC, nImage, rcItem.TopLeft(), ILD_NORMAL);
				 rcItem.left += nImageWidth;
			}

		}
		if (lpdis->itemID >=0)
			OnDrawThisText(pDC, rcItem, lpdis->itemID);
		pDC->DrawFocusRect(&rcItem);
 		if (m_bHasExplanations){
			rcBtn.SetRect(rcItem.right - 17, rcItem.top, rcItem.right, rcItem.bottom);
			pDC->DrawFrameControl(&rcBtn, DFC_MENU, DFCS_MENUARROW);
		}

		break;

	case IS_NORMAL:
		pDC->SetTextColor(::GetSysColor(COLOR_WINDOWTEXT));
		pDC->SetBkColor(::GetSysColor(COLOR_WINDOW));
		pDC->FillRect(&lpdis->rcItem, &CBrush(::GetSysColor(COLOR_WINDOW)) );
		if ((m_bHasIcons)&&(lpdis->itemID <3)){
			IMAGEINFO Info;
			int nImage = (lpdis->itemID + 2)%3;
			if (m_pParent->GetImageList()->GetImageInfo(nImage, &Info)) 
			{ 
				 int nImageWidth = Info.rcImage.right-Info.rcImage.left+1;
				 m_pParent->GetImageList()->Draw(pDC, nImage, rcItem.TopLeft(), ILD_NORMAL);
				 rcItem.left += nImageWidth;
			}
		}
		if (lpdis->itemID >=0)
			OnDrawThisText(pDC, rcItem, lpdis->itemID);
		if (m_bHasExplanations){
			rcBtn.SetRect(rcItem.right - 17, rcItem.top, rcItem.right, rcItem.bottom);
			pDC->DrawFrameControl(&rcBtn, DFC_BUTTON, DFCS_FLAT|DFCS_ADJUSTRECT);
		}

		break;
	}
}

CRect CInPlaceList::OnDrawThisText(CDC* pDC, CRect rcItem, int itemID)
{
	CRect rcText = rcItem;
	CRect rcBorder = rcItem;
	rcText.SetRect(rcText.left+3, rcText.top, rcText.right-3, rcText.bottom);
	rcBorder.SetRect(rcItem.left+1, rcItem.top, rcItem.right-1, rcItem.bottom);
	int wid = 0;
 	CString str;
	wid = DrawThisText(pDC, rcText, 0, itemID);
	return rcBorder;

}

int CInPlaceList::DrawThisText(CDC* pDC, CRect rcText, int nColumn, int nItem)
{
	CString str;
	GetText(nItem, str);
	pDC->DrawText(str, -1, rcText, DT_SINGLELINE| DT_NOPREFIX| 
		DT_NOCLIP | DT_VCENTER);
	return 1;
}


void CInPlaceList::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CRect rect;
	GetClientRect(&rect);
	if (!rect.PtInRect(point))
	{
		m_bCancel = TRUE;
		EndSelect();
		return;
	}
	CListBox::OnLButtonDown(nFlags, point);
}

int CInPlaceList::HitTest(int nIndex, CPoint point)
{
	CRect rect;
	if (GetItemRect(nIndex, &rect)){
		CRect arrowRect = CRect(rect.right-14, rect.top, rect.right, rect.bottom);
		if (arrowRect.PtInRect(point))
			return LBN_ARROW;
		else if (rect.PtInRect(point))
			return LBN_TEXT;
	}
	return 0;
}


void CInPlaceList::PostNcDestroy() 
{

 
    // Send a message to the parent of this edit's parent, telling the parent's parent
    // that the parent of this edit ctrl has recieved a LVN_ENDLABELEDIT message. 
    // Makes perfect sense, no? :)
    m_pParent->SendMessage(WM_NOTIFY, m_pParent->GetDlgCtrlID(), (LPARAM)&m_dispinfo );
	//this call needs to be here to ensure that the list box is destroyed, otherwise
	//we get caught in a handler and the list box remains up
	
	CListBox::PostNcDestroy();
}
