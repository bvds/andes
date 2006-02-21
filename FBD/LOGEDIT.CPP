// LogEdit.cpp : definitions for subclassed edit and combo controls that log
// changes to history file. 
//
// We make use of MFC message reflection to intercept notification messages normally
// sent to the parent and process them in the control class itself. Thus the logging code 
// only has to be written in one place, rather then scattered throughout every possible 
// container.
//
//

#include "stdafx.h"
#include "fbd.h"
#include "fbddoc.h"
#include "LogEdit.h"
#include "history.h"
#include "Lgdialog.h"
#include "GreekOpts.h" // for LogRichCombo

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLogEdit
IMPLEMENT_DYNAMIC(CLogEdit, CEdit)

CLogEdit::CLogEdit()
{
	m_status = statusUnknown;
	m_bEnabled = TRUE;
}

CLogEdit::~CLogEdit()
{
}


BEGIN_MESSAGE_MAP(CLogEdit, CEdit)
	//{{AFX_MSG_MAP(CLogEdit)
	ON_CONTROL_REFLECT_EX(EN_CHANGE, OnChange)
	ON_CONTROL_REFLECT_EX(EN_SETFOCUS, OnSetfocus)
	ON_WM_RBUTTONDOWN()
	ON_WM_CONTEXTMENU()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogEdit message handlers

BOOL CLogEdit::OnSetfocus() 
{
	int ctrlId = GetDlgCtrlID();
	LogEventf(EV_FOCUS, "%s", ((CLogDialog*)GetParent())->GetCtlName(this));

	theApp.SendTrainer(this, ((CLogDialog*)GetParent())->GetTrainerId(ctrlId) );
	return FALSE;
}

// Log change in edit control text.
BOOL CLogEdit::OnChange() 
{
	// Currently dump whole text to log file. 
	CString strText;
	GetWindowText(strText);
    // !!! Need to handle logging of long text objects properly.
	// Log messages are single line records. But edit box text can be 
	// multi-line; also can be so long as to overflow the log
	// message buffer. 
	LogEventf(EV_CTL_CHANGE, "%s %s", ((CLogDialog*)GetParent())->GetCtlName(this), (LPCTSTR) strText);
	return FALSE;
}

void CLogEdit::SetStatus(Status status)
{
	m_status = status;
	Invalidate();
}

void CLogEdit::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_DIALOG));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);


	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);	
	
}

void CLogEdit::OnRButtonDown(UINT nFlags, CPoint point) 
{
	SetFocus();
	
	CEdit::OnRButtonDown(nFlags, point);
}

/////////////////////////////////////////////////////////////////////////////
// CLogRichEdit
IMPLEMENT_DYNAMIC(CLogRichEdit, CRichEditCtrl)

CLogRichEdit::CLogRichEdit()
{}

CLogRichEdit::~CLogRichEdit()
{}

BEGIN_MESSAGE_MAP(CLogRichEdit, CRichEditEx)
	//{{AFX_MSG_MAP(CLogRichEdit)
	ON_CONTROL_REFLECT_EX(EN_CHANGE, OnChange)
	ON_CONTROL_REFLECT_EX(EN_SETFOCUS, OnSetfocus)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogRichEdit message handlers

BOOL CLogRichEdit::OnSetfocus() 
{
	// don't log if we're not contained in a log dialog (for eq view)
	if (! GetParent()->IsKindOf(RUNTIME_CLASS(CLogDialog))) return FALSE;

	LogEventf(EV_FOCUS, "%s", ((CLogDialog*)GetParent())->GetCtlName(this));

	int ctrlId = GetDlgCtrlID();
	theApp.SendTrainer(this, ((CLogDialog*)GetParent())->GetTrainerId(ctrlId) );
	return FALSE;
}

// Log change in edit control text.
BOOL CLogRichEdit::OnChange() 
{
	// don't log if we're not contained in a log dialog (for eq view)
	if (! GetParent()->IsKindOf(RUNTIME_CLASS(CLogDialog))) return FALSE;

	// Currently dump whole text to log file. 
	CString strText;
	GetRichEditText(strText);
    // !!! Need to handle logging of long text objects properly.
	// Log messages are single line records. But edit box text can be 
	// multi-line; also can be so long as to overflow the log
	// message buffer. 
	LogEventf(EV_RICHEDIT_CHANGE, "%s %s", ((CLogDialog*)GetParent())->GetCtlName(this), (LPCTSTR) strText);
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// CLogCombo
IMPLEMENT_DYNAMIC(CLogCombo, CComboBox)
CLogCombo::CLogCombo()
{
	m_status = statusUnknown;
	m_bEnabled = TRUE;
}

CLogCombo::~CLogCombo()
{
}


BEGIN_MESSAGE_MAP(CLogCombo, CComboBox)
	//{{AFX_MSG_MAP(CLogCombo)
	ON_CONTROL_REFLECT_EX(CBN_EDITCHANGE, OnEditchange)
	ON_CONTROL_REFLECT_EX(CBN_DROPDOWN, OnDropdown)
	ON_CONTROL_REFLECT_EX(CBN_SELCHANGE, OnSelchange)
	ON_CONTROL_REFLECT_EX(CBN_CLOSEUP, OnCloseup)
	ON_CONTROL_REFLECT_EX(CBN_SETFOCUS, OnSetfocus)
	ON_WM_CONTEXTMENU()
	ON_WM_RBUTTONDOWN()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogCombo message handlers
//
// Log significant events in combo box class
//
// NB we don't record user scrolling among the items in the list, since no 
// notifications 

// We got the focus
BOOL CLogCombo::OnSetfocus() 
{
	int ctrlId = GetDlgCtrlID();
	LogEventf(EV_FOCUS, "%s", ((CLogDialog*)GetParent())->GetCtlName(this));

	theApp.SendTrainer(this, ((CLogDialog*)GetParent())->GetTrainerId(ctrlId) );
	
/*	// For convenience, we show the box to the user
	ShowDropDown(); // doesn't work for mouse click focussing */
	return FALSE;
}

// User dropped down the list
BOOL CLogCombo::OnDropdown() 
{
	LogEventf(EV_CBO_DROP, "%s", ((CLogDialog*)GetParent())->GetCtlName(this));
	CWnd* pWnd = GetParent();
	return FALSE;
}

// User changed the value of the combo box. This can happen even if list is
// not dropped through use of arrow keys
BOOL CLogCombo::OnSelchange() 
{
	CString strText;
	GetWindowText(strText);

	// 2/19/99: change to add the string value. (was TopIndex by mistake; useless, always 0)
	// This is more useful than index, which could change if list changes.
	// However, might be an issue for backwards compatibility -- still want to
	// be able to playback old logs.
	LogEventf(EV_CBO_SEL, "%s %d %s", ((CLogDialog*)GetParent())->GetCtlName(this), 
		                     GetCurSel(), strText);
	return FALSE;
}

#if 0	// no need to log this event at all, if all sel changes properly logged 
//
// User ended selection mode on an item in a way that OK'd it. 
// (Not clear there's any point it logging this, we will get the value in any
// case on the Selchange event if changed.) 
BOOL CLogCombo::OnSelendok() 
{

	CString strText;
	GetWindowText(strText);
	// Earlier version had TopIndex as third arg, to show user scrolled in droplist. 
	// taken out since it is of little use to us. 
	// !! change might be problem if ever need to parse this msg in old logs
	/* LogEventf(EV_CBO_SOK, "%s %s %d", ((CLogDialog*)GetParent())->GetCtlName(this), strText, GetTopIndex()) */
	LogEventf(EV_CBO_SOK, "%s %s", ((CLogDialog*)GetParent())->GetCtlName(this), strText);
	
	return FALSE;
}
#endif 0

// User closed up the combo box
BOOL CLogCombo::OnCloseup() 
{
	CString strText;
	GetWindowText(strText);

	// include current value at closeup for information.
	LogEventf(EV_CBO_CLOSE, "%s %s", ((CLogDialog*)GetParent())->GetCtlName(this), strText);
	return FALSE;
}

//
// Following not used for drop-down lists, only if user can also type in value.
// Currently none of our combos are of that style.
//
BOOL CLogCombo::OnEditchange() 
{
	CString strText;
	GetWindowText(strText);

	LogEventf(EV_CTL_CHANGE, "%s %s", ((CLogDialog*)GetParent())->GetCtlName(this), strText);
	return FALSE;
	
}

void CLogCombo::SetStatus(Status status)
{
	m_status = status;
	Invalidate();
}

void CLogCombo::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_DIALOG));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);


	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);	
	
}

void CLogCombo::OnRButtonDown(UINT nFlags, CPoint point) 
{
	SetFocus();
	
	CComboBox::OnRButtonDown(nFlags, point);
}

int CLogCombo::SelectStringExact(CString strFind )
{

	int nIndex = FindStringExact(-1, strFind);
	if (nIndex >= 0)
		return SetCurSel(nIndex);
	return -1;
}

/////////////////////////////////////////////////////////////////////////////
// CLogRichCombo -- Logging combo enabling Greek text in items
//
// duplicates owner-draw handling code from CRichCombo

CLogRichCombo::CLogRichCombo()
{
}

CLogRichCombo::~CLogRichCombo()
{
}

BEGIN_MESSAGE_MAP(CLogRichCombo, CLogCombo)
	//{{AFX_MSG_MAP(CLogRichCombo)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogRichCombo message handlers

void CLogRichCombo::MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct) 
{
	lpMeasureItemStruct->itemHeight = 16;
	
}

// state codes passed to DrawDropList:
static const UINT IS_NORMAL =	0x1000;
static const UINT IS_HIGHLIGHTED =	0x1001;

void CLogRichCombo::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct) 
{
	if (lpDrawItemStruct->itemID >= 20)
		return;

	BOOL bFocus = (lpDrawItemStruct->itemAction & ODA_FOCUS);
	BOOL bSelected = (lpDrawItemStruct->itemState & ODS_SELECTED);

	if (bSelected)
		DrawDropList(lpDrawItemStruct, IS_HIGHLIGHTED);
	else
		DrawDropList(lpDrawItemStruct, IS_NORMAL);	
}

void CLogRichCombo::DrawDropList(LPDRAWITEMSTRUCT lpdis, UINT nState)
{
	CDC* pDC = CDC::FromHandle(lpdis->hDC);
	CRect rcItem(lpdis->rcItem);

	switch (nState){
	case IS_HIGHLIGHTED:
		pDC->SetTextColor(::GetSysColor(COLOR_HIGHLIGHTTEXT));
		pDC->SetBkColor(::GetSysColor(COLOR_HIGHLIGHT));
		pDC->FillRect(&lpdis->rcItem, &CBrush(::GetSysColor(COLOR_HIGHLIGHT)) );
		if (lpdis->itemID >=0)
			OnDrawThisText(pDC, rcItem, lpdis->itemID);
		pDC->DrawFocusRect(&rcItem);

		break;

	case IS_NORMAL:
		pDC->SetTextColor(::GetSysColor(COLOR_WINDOWTEXT));
		pDC->SetBkColor(::GetSysColor(COLOR_WINDOW));
		pDC->FillRect(&lpdis->rcItem, &CBrush(::GetSysColor(COLOR_WINDOW)) );

		if (lpdis->itemID >=0)
			OnDrawThisText(pDC, rcItem, lpdis->itemID);

		break;
	}
}

CRect CLogRichCombo::OnDrawThisText(CDC* pDC, CRect rcItem, int itemID)
{
	CRect rcText = rcItem;
	CRect rcBorder = rcItem;
	rcText.SetRect(rcText.left+3, rcText.top, rcText.right-3, rcText.bottom);
	rcBorder.SetRect(rcItem.left+1, rcItem.top, rcItem.right-1, rcItem.bottom);

	CString str;
	GetLBText(itemID, str);
	return CGreekText::DrawText(pDC, rcText, str);

}

/////////////////////////////////////////////////////////////////////////////
// CLogList
IMPLEMENT_DYNAMIC(CLogList, CListBox)

CLogList::CLogList()
{
	m_nSelected = 0;
	m_status = statusUnknown;
	m_bEnabled = TRUE;
}

CLogList::~CLogList()
{
}


BEGIN_MESSAGE_MAP(CLogList, CListBox)
	//{{AFX_MSG_MAP(CLogList)
	ON_CONTROL_REFLECT_EX(LBN_SELCHANGE, OnSelchange)
	ON_CONTROL_REFLECT_EX(LBN_SETFOCUS, OnSetfocus)
	ON_WM_CONTEXTMENU()
	ON_WM_RBUTTONDOWN()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogList message handlers

BOOL CLogList::OnSelchange() 
{
	CString strText;
	if (! (GetStyle() & LBS_MULTIPLESEL)) {	// single-selection box
		GetText(GetCurSel(), strText);
		LogEventf(EV_LIST_SEL, "%s %d %s", ((CLogDialog*)GetParent())->GetCtlName(this),
									    GetCurSel(), strText);
	} else { // multiple selection box
		// No window text returned, and CurSel is index of item with keyboard focus. 
		// That is *usually* the item whose selection state was just toggled, but not 
		// always -- also get this notification when just move focus with arrow keys 
		// without changing selection (can toggle focus item selection with space-bar)
		// So must check if selection status changed since last time
		int iFocus = GetCurSel();
		BOOL bNowSelected = GetSel(iFocus);
		// lookup in saved selection set (set of indices).
		BOOL bWasSelected = FALSE;
		for (int i = 0; i < m_nSelected; i++) {
			if (m_Selected[i] == iFocus) {
				bWasSelected = TRUE;
				break;
			}
		}	
		
		// log only if focus item's selection state changed
		if (bNowSelected != bWasSelected) {
			GetText(iFocus, strText);
			LogEventf(EV_LIST_SEL, "%s %d %s", ((CLogDialog*)GetParent())->GetCtlName(this),
									    iFocus, strText);
			// update saved selection set
			m_nSelected = GetSelCount();		// number of items selected
			ASSERT(m_nSelected < MAXSEL);
			GetSelItems(MAXSEL, m_Selected);
		}
	}
	return FALSE;
}

BOOL CLogList::OnSetfocus() 
{
	int ctrlId = GetDlgCtrlID();
	LogEventf(EV_FOCUS, "%s", ((CLogDialog*)GetParent())->GetCtlName(this));

	theApp.SendTrainer(this, ((CLogDialog*)GetParent())->GetTrainerId(ctrlId) );
	
	return FALSE;
}

void CLogList::SetStatus(Status status)
{
	m_status = status;
	Invalidate();
}



void CLogList::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	CMenu menu;
	VERIFY(menu.LoadMenu(IDR_POPUP_DIALOG));
	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);


	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);	
	
}

void CLogList::OnRButtonDown(UINT nFlags, CPoint point) 
{
	SetFocus();
	
	CListBox::OnRButtonDown(nFlags, point);
}

/////////////////////////////////////////////////////////////////////////////
// CLogBtn
IMPLEMENT_DYNAMIC(CLogBtn, CButton)

CLogBtn::CLogBtn()
{
}

CLogBtn::~CLogBtn()
{
}


BEGIN_MESSAGE_MAP(CLogBtn, CButton)
	//{{AFX_MSG_MAP(CLogBtn)
	ON_CONTROL_REFLECT_EX(BN_CLICKED, OnClicked)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLogBtn message handlers

BOOL CLogBtn::OnClicked() 
{
	CString strText;
	GetWindowText(strText);
	// No need to use control name, button text will identify for human reader
	LogEventf(EV_BTN_CLICK, "%d %s", GetDlgCtrlID() , (LPCTSTR) strText);

	theApp.SendTrainer(this, 0, HELP_QUIT);
	
	return FALSE;
}


