/////////////////////////////////////////////////////////////////////////////
// EQEdit -- Subclassed edit controls for equations
// 
// Defines classes derived from both normal and richedit controls. Currently
// there are slight differences in usage between them. E.g. richedits cannot
// be placed on dialogs in VC4.0, so must be created to replace dialog controls.
//
// Currently these are still fairly generic edit controls that only add methods for 
// setting color and for hooking context menu creation. The controls don't actually 
// know about status values, just colors, nor about data exchange with document data
// structures, nor about help system. All that is left to containing views or dialogs.
//
#include "stdafx.h"
#include "FBD.h"
#include "FBDDoc.h"
#include "history.h"
#include "EQView.h"
#include "EQEdit.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define EM_POSFROMCHAR          0x00D6
#define EM_CHARFROMPOS          0x00D7

// CEQEdit: Normal edit control derivative
//
// Adds methods to set text and background colors.
// Also uses equation view's popup menu.
//
CEQEdit::CEQEdit()
{
	m_colorText = ::GetSysColor(COLOR_WINDOWTEXT); // default to black
	// A nicety: use system colors if we can. We leave our background brush
	// w/its initial null background brush handle => default to system background 
	// color. A non-NULL brush handle => user has set a custom background color. 
	// m_colorBkgnd only valid in case we have a custom background color.

	m_nMenuId = -1;
}

BOOL CEQEdit::PreCreateWindow(CREATESTRUCT& cs) 
{
	// TODO: Add your specialized code here and/or call the base class
	// Need to specify client edge for sunken border if not in dialog
	cs.dwExStyle |= WS_EX_CLIENTEDGE;
	return CEdit::PreCreateWindow(cs);
}

CEQEdit::~CEQEdit()
{
}

void CEQEdit::SetTextColor(COLORREF color)
{
	m_colorText = color;
	Invalidate();
}

void CEQEdit::SetBkColor(COLORREF color) // sets a custom background color
{
	m_colorBkgnd = color;
	if ((HBRUSH) m_brBkgnd)
		m_brBkgnd.DeleteObject();
	m_brBkgnd.CreateSolidBrush(color);
	Invalidate();
}

/////////////////////////////////////////////////////////////////////////////
// CEQEdit message handlers

BEGIN_MESSAGE_MAP(CEQEdit, CEdit)
	//{{AFX_MSG_MAP(CEQEdit)
	ON_WM_CONTEXTMENU()
	ON_WM_CTLCOLOR_REFLECT()
	ON_WM_CHAR()
	ON_WM_RBUTTONDOWN()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

HBRUSH CEQEdit::CtlColor(CDC* pDC, UINT nCtlColor) 
{
	// TODO: Change any attributes of the DC here
	// Set our text color
	pDC->SetTextColor (m_colorText);

	// TODO: Return a non-NULL brush if the parent's handler should not be called
	// Ifwe are read-only or disabled, use system default 3D object color
	if (nCtlColor == CTLCOLOR_STATIC) 
	{
		pDC->SetBkColor(::GetSysColor (COLOR_3DFACE));
		return ::GetSysColorBrush (COLOR_3DFACE);
	}
	else if ((HBRUSH) m_brBkgnd == NULL) // no custom background, use defaults
	{ 
		pDC->SetBkColor (::GetSysColor(COLOR_WINDOW));
		return ::GetSysColorBrush (COLOR_WINDOW);
	}
	// else we have custom background color
	pDC->SetBkColor(m_colorBkgnd);
	
	return (HBRUSH) m_brBkgnd;
}

// Windows default processing (in DefWindowProc) treats right-button up as an accelerator, 
// translating it to a WM_CONTEXTMENU msg. This msg will be sent to child window controls if 
// they have the focus. Edit controls normally process it and show their built-in context
// menu. To allow our client to customize the context menu, we trap the message and show 
// client-set menu in place of the edit control's built-in one. If we don't have focus, 
// parent window gets the message instead.
// 
// An alternative would be to forward the message to the parent window for handling,
// Would have to build a WM_CONTEXTMENU msg with parms fetched from GetCurrentMessage.
// (Can't simply call parent CWnd's OnContextMenu, mapped WM* handlers are protected.)
void CEQEdit::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	// if client hasn't set Id, use default handling
	if (m_nMenuId == -1) {
		CEdit::Default();
		return;
	}

	CMenu menu;
	VERIFY(menu.LoadMenu(m_nMenuId));

	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);
}

// Grab focus on Right button down so we will have it when release 
// generates context menu. Could also do in OnContextMenu, but this way user
// gets feedback that focus has changed when mouse goes down
void CEQEdit::OnRButtonDown(UINT nFlags, CPoint point) 
{
	SetFocus();
	
	CEdit::OnRButtonDown(nFlags, point);
}

// We will get RETURN if not inside a dialog which manages keyboard interface.
// Trap it and send message to parent coding SUBMIT command.
void CEQEdit::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if (nChar == VK_RETURN)	
	{
		// simulate click of an OK button, which parent should treat as SUBMIT.
		// We put edit's hWnd in the LPARAM, though. (Not clear if it matters).
		GetParent()->SendMessage(WM_COMMAND, MAKELONG(IDOK, BN_CLICKED), (LPARAM) m_hWnd);
		return;
	}
	
	CEdit::OnChar(nChar, nRepCnt, nFlags);
}



/////////////////////////////////////////////////////////////////////////////
// CEQRichEdit -- rich edit control derivative

CEQRichEdit::CEQRichEdit()
{
	m_colorText = ::GetSysColor(COLOR_WINDOWTEXT);
	m_bParentMenu = FALSE;
	m_nMenuId = -1;
	m_bFormatChange = 0;
}

CEQRichEdit::~CEQRichEdit()
{
}


BEGIN_MESSAGE_MAP(CEQRichEdit, CLogRichEdit)
	//{{AFX_MSG_MAP(CEQRichEdit)
	ON_WM_CONTEXTMENU()
	ON_WM_RBUTTONUP()
	ON_WM_MOUSEACTIVATE()
	ON_WM_CHAR()
	ON_WM_RBUTTONDOWN()
	ON_WM_LBUTTONDOWN()
	ON_CONTROL_REFLECT_EX(EN_CHANGE, OnChange)
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
	ON_NOTIFY_REFLECT(EN_PROTECTED, OnProtected)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEQRichEdit message handlers
IMPLEMENT_DYNAMIC(CEQRichEdit, CLogRichEdit)

BOOL CEQRichEdit::PreCreateWindow(CREATESTRUCT& cs) 
{
	// Need to specify client edge for sunken border.
	cs.dwExStyle |= WS_EX_CLIENTEDGE;
	return CRichEditCtrl::PreCreateWindow(cs);
}

// Following override is just as for normal edits. But it looks like RichEditCtrl 
// processes RightButtonUp, to show its context menu, and never actually handles a 
// WM_CONTEXT_MENU message. We override RightButtonUp as well.
void CEQRichEdit::OnContextMenu(CWnd* pWnd, CPoint point) 
{
	// see if parent wants to do the work
	if (m_bParentMenu) {
		GetParent()->SendMessage(WM_CONTEXTMENU, (WPARAM) m_hWnd, MAKELPARAM(point.x, point.y));
		return;
	}

	// if client hasn't set custom menu, just use default processing
	if (m_nMenuId == -1) {
		CWnd::Default();
		return;
	}

	CMenu menu;
	VERIFY(menu.LoadMenu(m_nMenuId));

	CMenu* pPopup = menu.GetSubMenu(0);
	ASSERT(pPopup != NULL);

	CWnd* pWndPopupOwner = this;
	while (pWndPopupOwner->GetStyle() & WS_CHILD)
		pWndPopupOwner = pWndPopupOwner->GetParent();

	pPopup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON, point.x, point.y,
		pWndPopupOwner);
}


void CEQRichEdit::OnRButtonUp(UINT nFlags, CPoint point) 
{
	// Override control's RButtonUp processing to show client's custom context menu
	ClientToScreen(&point);
	OnContextMenu(this, point); 
}


void CEQRichEdit::OnRButtonDown(UINT nFlags, CPoint point) 
{
	// Grab focus on Right button down so we will have it when release 
	// generates context menu.  Else we could be showing context menu
	// when focus is in another edit control, and commands like HelpWhatsWrong will be
	// enabled based on focus window. 
	// Could just grab it in OnContextMenu, but this way user
	// gets feedback that focus has changed when mouse goes down.
	SetFocus();
	
	CRichEditCtrl::OnRButtonDown(nFlags, point);
}

// Windows sends this message on mouse clicks in an inactive window.
// Normally seems to be passed to DefWindowProc which sends it to the parent
// (a sort of notification). Looks like MFC catches it and uses to activate
// the containing view, which is important for command UI enabling.
// But this is not happening with our richedit. So we trap it and explicitly 
// forward it to the parent here. Urgh.
// Turns out KB article Q166213 documents this as a bug in CRichEditCtrl and
// gives the following code:
int CEQRichEdit::OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message) 
{
	const MSG* pMsg = GetCurrentMessage();
	GetParent()->SendMessage(WM_MOUSEACTIVATE, pMsg->wParam, pMsg->lParam);

    return CRichEditCtrl::OnMouseActivate(pDesktopWnd, nHitTest, message);
}

void CEQRichEdit::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if (nChar == VK_RETURN)	
	{
		// simulate click of an OK button, which parent should treat as SUBMIT.
		// We put edit's hWnd in the LPARAM, though. (Not clear if it matters).
		GetParent()->SendMessage(WM_COMMAND, MAKELONG(IDOK, BN_CLICKED), (LPARAM) m_hWnd);
		return;
	}
	//Force no symbol font from keyboard
	//only if printable character (ie not CTRL-A (select all) or Tabbing)
	if (isprint(nChar))
		SetCharNoSymbol();

	CRichEditCtrl::OnChar(nChar, nRepCnt, nFlags);
}

void CEQRichEdit::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CRichEditCtrl::OnLButtonDown(nFlags, point);
	LONG nSt, nEnd;
	GetSel(nSt, nEnd);
	if (nSt == nEnd)
	{
		// whenever students clicks to place cursor, the character formats
		// are cleared, we may not want this

		// suppress EN_CHANGE notifications around this format change
//		DWORD dwOldEventMask = GetEventMask();	
//		SetEventMask(dwOldEventMask & ~ENM_CHANGE);
		// the above is now handled in OnProtected which is called before OnChange

		SetCharPlain();
		
//		SetEventMask(dwOldEventMask);	
	}
}

// process our keboard "accelerators". Should pre-empt application's.
BOOL CEQRichEdit::PreTranslateMessage(MSG* pMsg) 
{
	if ((pMsg->message == WM_KEYDOWN)  &&	// user pressed a key and
		(::GetKeyState(VK_CONTROL) < 0))	// control modifier is down
	{
		switch(pMsg->wParam) 									
		{
		case 'b': case 'B': 
			ToggleCharBold();
			return TRUE;
		
		case 'i': case 'I':
			ToggleCharItalic();
			return TRUE;
		
	//	case 's': case 'S':	//don't allow symbol type because no way
	//		SetCharSymbol();//to control RichEditCtrl's arbitrary 
	//		return TRUE;//font decisions.  Don't want student stuck in
						//Greekland
		case 'p': case 'P':
			SetCharPlain();
			return TRUE;

		default: 
			break;	// continue on
		}
	}
	else if (pMsg->message == WM_SYSKEYDOWN)	// user pressed a key and
		// ALT modifier is down
	{//still want a way to type Greek (with shortcut key)
		//using ALT + key
		CString alpha = "abcdefghijklmnopqrstuvwxyz";//Use VkKey scan to
		int pos = (int)pMsg->wParam - VkKeyScan('a');//convertASCII to
		// TRACE("EQRichEdit got ALT-key: WParam=%d\n", pMsg->wParam);		//virtual key code
		if ((pos < 26)&&(pos >= 0)){
			CString letter = alpha[pos];
			CString gralpha = "abgdezhqiklmnxoprstufcyw";//greek translation
			int grpos = gralpha.Find(letter);
			if (grpos != -1){
				UINT msg = (UINT)grpos + IDM_GREEKLETTER_FIRST;
				GetParent()->SendMessage(WM_COMMAND, msg);
			}
			return TRUE;
		}
	
	
	}

	return CRichEditCtrl::PreTranslateMessage(pMsg);
}

//
// The control sends EN_CHANGE notifications on changes in format. This caused
// problems when we tried to change the format programmatically, triggering a
// change notification, and the EN_CHANGE handler issued another format change 
// (setting color to black), which interfered with the format we were trying to set.
// So we suppress sending EN_CHANGE notifications for format changes as follows:
// Mark all control text as protected, so we can filter reflected protected change 
// notifications to flag when we are in a format change. Then filter reflected change 
// notifications and eat them if they're only format changes.
//
void CEQRichEdit::OnProtected(NMHDR* pNMHDR, LRESULT* pResult)
{
	ENPROTECTED* pEP = (ENPROTECTED*)pNMHDR;

	*pResult = 0;

	// flag format changes to suppress EN_CHANGE notifications for these (OnChange)
	switch (pEP->msg) {
	case EM_SETCHARFORMAT:
		m_bFormatChange = TRUE;
	break;
	default:
		m_bFormatChange = FALSE;
		break;
	};

}

BOOL CEQRichEdit::OnChange() 
{
	if (m_bFormatChange)//eaten if true; else msg continues to base class
		return TRUE;	//which may reflect to parent
	else
		return CLogRichEdit::OnChange();
}


//
// Format manipulation helpers:
//
void CEQRichEdit::SetTextColor(COLORREF color)
{
	// Use char formatting to set color for all text in control
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	cf.dwMask = CFM_COLOR;	// NB: validates crTextColor *plus* CFE_AUTOCOLOR bit in dwEffects
	cf.crTextColor = color;
	cf.dwEffects = 0;		// so must clear CFE_AUTOCOLOR bit (fixes earlier bug).		

	// select everything in control and set its color
	CHARRANGE crOldSel;		// saves current selection
	GetSel(crOldSel);
	HideSelection(TRUE, FALSE);		// temp. turns off selection highlighting
	SetSel(0, -1);					// selects all 
	SetSelectionCharFormat (cf);
	SetSel(crOldSel);				// restores old selection
	HideSelection(FALSE, FALSE);	// restores selection highlighting

	//????????????????
	//Commented out following lines because whenever 
	//a greek letter was at place zero on the line
	//the default formatting changed it back to regular font

	// also set default color to use for new test.
//	SetDefaultCharFormat(cf);
}

void CEQRichEdit::SetCharNoSymbol()
{
	// If EN_CHANGE notifications requested, control sends them even on format
	// changes. Suppress them around this change, which is always program-caused.
//	DWORD dwOldEventMask = GetEventMask();	
//	SetEventMask(dwOldEventMask & ~ENM_CHANGE);
	//the above is now handled in OnProtected which is called before OnChange
// undo char format effects
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);

	// restore default face (from default charformat, which we never change.)
	CHARFORMAT cfDefault;
	cfDefault.cbSize = sizeof(CHARFORMAT);
	GetDefaultCharFormat(cfDefault);
	::lstrcpy(cf.szFaceName, cfDefault.szFaceName); 
	cf.dwMask = CFM_FACE;
	SetSelectionCharFormat(cf);

	// restore original event mask
//	SetEventMask(dwOldEventMask);	
}


int CEQRichEdit::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CRichEditCtrl::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	// Move this into subclass?
	// Ctl requires text be protected to block EN_CHANGE on format changes
/*	CHARFORMAT cfDefault;
	cfDefault.cbSize = sizeof(cfDefault);
	cfDefault.dwEffects = CFE_PROTECTED; 
	cfDefault.dwMask = CFM_PROTECTED;
	SetDefaultCharFormat(cfDefault);

	// Set up to get change notifications. Must OR in ENM_PROTECTED since 
	// control requires it. 
	SetEventMask(ENM_CHANGE|ENM_PROTECTED);*/
	
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CLabelRichEdit -- rich edit custom control
/////////////////////////////////////////////////////////////////////////////
// CLabelRichEdit

IMPLEMENT_DYNAMIC(CLabelRichEdit, CEQRichEdit)

CLabelRichEdit::CLabelRichEdit()
{
	m_chrgPrefix.cpMin = 0;
	m_chrgPrefix.cpMax = 0;
	m_bSettingPrefix = FALSE;
}

CLabelRichEdit::~CLabelRichEdit()
{
}


BEGIN_MESSAGE_MAP(CLabelRichEdit, CEQRichEdit)
    //{{AFX_MSG_MAP(CLabelRichEdit)
	ON_WM_LBUTTONDBLCLK()
	ON_WM_CREATE()
	ON_WM_SETFOCUS()
	//}}AFX_MSG_MAP
	ON_NOTIFY_REFLECT(EN_MSGFILTER, OnMsgFilter)
	ON_NOTIFY_REFLECT(EN_LINK, OnPrefix)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLabelRichEdit message handlers
void CLabelRichEdit::SetPrefix(CString strPrefix)
{
	if (strPrefix.IsEmpty())
		return;
	
	m_strPrefix = strPrefix;

	long nOldStart, nOldEnd;
	GetSel(nOldStart, nOldEnd);
	// save sel relative to start of non-prefix text
	nOldStart -= m_chrgPrefix.cpMax;
	nOldEnd -= m_chrgPrefix.cpMax;
	//HideSelection(TRUE, FALSE);		// temp. turns off selection highlighting

	// Select old prefix range (will be 0,0 if no prefix has been set).
	SetSel(m_chrgPrefix);

	// ensure prefix format set to bold and set link attribute
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	cf.dwEffects = CFE_BOLD | CFE_LINK;
	cf.dwMask =	CFM_BOLD |  CFM_LINK | CFM_COLOR;
	cf.crTextColor = RGB(128,128,128); 
	SetSelectionCharFormat(cf);
	
	// Replace selection with new prefix text. Leaves insertion pt at end of prefix
	m_bSettingPrefix = TRUE;
	SetRichEditText(strPrefix);	// operates on current selection in this case
	m_bSettingPrefix = FALSE;
#if 0
	long nStart, nEnd;
	GetSel(nStart, nEnd);
	TRACE("Sel after set prefix=(%d, %d)\n", nStart, nEnd);
#endif
	// Update prefix range for new prefix
	// Note prefix (and text) strings may contain $ tags to indicate greek, so
	// their length may be greater then number of displayed chars in edit control. 
	// But chrgPrefix should always have position just after prefix.
	CString strPrefixChars = m_strPrefix;
	strPrefixChars.Remove('$');
	m_chrgPrefix.cpMax = strPrefixChars.GetLength();

	//set format on insertion point after new prefix to non-bold non-link
	cf.dwEffects = CFE_AUTOCOLOR;	
	SetSel(m_chrgPrefix.cpMax, m_chrgPrefix.cpMax);
	SetSelectionCharFormat(cf);

	// restore old selection relative to new prefix
	SetSel(m_chrgPrefix.cpMax + nOldStart, m_chrgPrefix.cpMax + nOldEnd);			
	// HideSelection(FALSE, FALSE);	// restores selection highlighting
}


void CLabelRichEdit::OnMsgFilter(NMHDR* pNMHDR, LRESULT* pResult)
{
	*pResult = 0;

	MSGFILTER* pMsgFilter = (MSGFILTER*)pNMHDR;

	if (pMsgFilter->msg == WM_KEYDOWN)
	{
		TRACE("CLabelRichEdit::OnMsgFilter: keydown %d\n", pMsgFilter->wParam);
		LONG nBeg, nEnd;
		GetSel(nBeg, nEnd);

		if ((pMsgFilter->wParam == VK_BACK) || (pMsgFilter->wParam == VK_LEFT))
		{
			if (GetKeyState(VK_CONTROL) & 0x8000) 
			{
				nBeg = GetLeftWord(nBeg);
			}

			if (nBeg <= m_chrgPrefix.cpMax)
			{
				if (pMsgFilter->wParam == VK_LEFT)
					SetSel(m_chrgPrefix.cpMax, m_chrgPrefix.cpMax);
				else{
					SetSel(m_chrgPrefix.cpMax, nEnd);
					Clear();
				}
				SendMessage(WM_HSCROLL, SB_PAGELEFT, 0);
				*pResult = 1;

			}
		}
		else if ((pMsgFilter->wParam == VK_HOME)  || (pMsgFilter->wParam == VK_PRIOR))
		{
			if (GetKeyState(VK_SHIFT) & 0x8000) 
				SetSel(m_chrgPrefix.cpMax, nEnd);
			else
				SetSel(m_chrgPrefix.cpMax, m_chrgPrefix.cpMax);
			SendMessage(WM_HSCROLL, SB_PAGELEFT, 0);
			*pResult = 1;
		}
		else if (pMsgFilter->wParam == VK_UP)
		{
			if (GetKeyState(VK_CONTROL) & 0x8000) 
				SetSel(m_chrgPrefix.cpMax, m_chrgPrefix.cpMax);
			SendMessage(WM_HSCROLL, SB_PAGELEFT, 0);
			*pResult = 1;
		}
	}
	// following would enable ALT-ch Greek shortcuts. This method translates into Greek letter insertion
	// command message sent to parent to handle, which must handle it for a letter to appear. Could be changed
	// to do the insertion directly (this way allows Greek palette to be used in dlg, see CAngleDlg.)
	// !!! Not working: OnMsgFilter not getting called at all for LabelRichEdits used for Given Value box in 
	// VariableDlg. Not sure why -- maybe EventMask initialization problem? 
#if 0  
	else if (pMsgFilter->msg == WM_SYSKEYDOWN)	// user pressed a key and
		// ALT modifier is down
	{//still want a way to type Greek (with shortcut key)
		//using ALT + key
		CString alpha = "abcdefghijklmnopqrstuwxyz";//Use VkKey scan to
		int pos = (int)pMsgFilter->wParam - VkKeyScan('a');//convertASCII to
		TRACE("CLabelRichEdit::OnMsgFilter: ALT-key: WParam=%d\n", pMsgFilter->wParam);		//virtual key code
		if ((pos < 26)&&(pos >= 0)){
			CString letter = alpha[pos];
			CString gralpha = "abgdezhqiklmnxoprstufcyw";//greek translation
			int grpos = gralpha.Find(letter);
			if (grpos != -1){
				UINT msg = (UINT)grpos + IDM_GREEKLETTER_FIRST;
				GetParent()->SendMessage(WM_COMMAND, msg);
			}
			*pResult = 1;
		}
	}
#endif 
}


void CLabelRichEdit::OnPrefix(NMHDR* pNMHDR, LRESULT* pResult)
{
	ENLINK* pPrefix = (ENLINK*)pNMHDR;
	//This function intercepts & eats all Mouse messages and set cursor message for
	//charrange having EN_LINK style
	*pResult = 1;

}

// Set entire contents of control 
// Used when initializing contents or playing back change events from log
void CLabelRichEdit::SetRichEditText(CString str)
{
	if (!m_bSettingPrefix) // skip this when using routine to set prefix
	{
		if (str.Find(m_strPrefix) == 0) { // str begins with prefix
			// so set rest of string (after prefix) to control contents
			str = str.Mid(m_strPrefix.GetLength());
			//  set selection to whole non-prefix portion
			SetSel(m_chrgPrefix.cpMax, GetTextLength());
		} else {	// prefix doesn't match (happens on old logs)
			// should clear prefix and its formatting
			// for now, just select all to clobber existing contents with new ones 
			SetSel(0, -1);	
		}
	}

	while (str.Find("$") != -1) 
	{
		int grkPos = str.Find('$');		//find greek letter
		int strlen = str.GetLength();
		if (grkPos >= (strlen-1))//if at last position, break
			break;
		if (grkPos != -1) 
		{// insert text before greek letter
			CString text = str.Left(grkPos);
			if (!text.IsEmpty()) 
				ReplaceSel(text);

			CString c = str[grkPos +1]; //make sure letter follows $
			if ( ((c >= "a") && ( c<="z"))||((c >= "A") && ( c<="Z")) 
				  || (c >= "\306") || (c<="\310")) // empty set, intersection or union
			{
				//ReplaceSel(c);		// insert greek letter at caret
				//SetCharSymbol();
				InsertGreekText(c);
				SetCharPlain();		// restore plain text font
				if (m_bSettingPrefix) // but use bold in prefix range 
					ToggleCharBold();
			}
			else
			{
				CString tempStr = "$" + c; //insert actual $ and what follows
				ReplaceSel(tempStr);
			}
			str = str.Mid(grkPos+2);
		}

	}

	// handle last bit of text
	if (! str.IsEmpty()) {
		SetCharPlain();		// restore plain text font
		if (m_bSettingPrefix) // but use bold in prefix range 
			ToggleCharBold();
		ReplaceSel(str);
	}
}


void CLabelRichEdit::OnLButtonDblClk(UINT nFlags, CPoint point) 
{
	// TODO: Add your message handler code here and/or call default
	
	//CEQRichEdit::OnLButtonDblClk(nFlags, point);
}

int CLabelRichEdit::GetLeftWord(int nBeg)
{ 
	if (nBeg <= m_chrgPrefix.cpMax)
	{
		return nBeg;
	}
	CString str;
	GetWindowText(str);

	CString strDelimiters= " ,. ";
	int npos = nBeg;

	//find last word end
	for (int i = npos-1; i >= m_chrgPrefix.cpMax; i--)
	{
		CString strChar = str[i];
		if (strChar.FindOneOf(strDelimiters) >= 0)
			continue;
		break;
	}	
	npos = i;
	if (npos == -1)
		npos = nBeg-1;
	
	//find prev delimiter
	for (int j = npos; j >= m_chrgPrefix.cpMax; j--)
	{
		CString strChar = str[j];
		if (strChar.FindOneOf(strDelimiters)  >= 0)
			break;
		continue;
	}

	npos = j;
	if (npos <= m_chrgPrefix.cpMax)
		npos = m_chrgPrefix.cpMax;
	else
		npos = npos + 1; //word begins after prev delim

	return npos;
}

void CLabelRichEdit::OnSetFocus(CWnd* pOldWnd) 
{
	CEQRichEdit::OnSetFocus(pOldWnd);
	LONG nBeg, nEnd;
	GetSel(nBeg, nEnd);
	if (nBeg < m_chrgPrefix.cpMax)
	{
		nBeg = m_chrgPrefix.cpMax;
	}
	if (nEnd < m_chrgPrefix.cpMax)
	{
		nEnd = m_chrgPrefix.cpMax;
	}
	SetSel(nBeg, nEnd);
	// TODO: Add your message handler code here
	
}
