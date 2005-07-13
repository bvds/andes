//RichEdit.cpp -- abstract base class for Rich edits

#include "stdafx.h"
#include "FBD.h"
#include "FBDDoc.h"
#include "history.h"
#include "RichEditEx.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CRichEditEx - the abstract base class for our rich edits

CRichEditEx::CRichEditEx()
{
}

CRichEditEx::~CRichEditEx()
{
}


BEGIN_MESSAGE_MAP(CRichEditEx, CRichEditCtrl)
	//{{AFX_MSG_MAP(CRichEditEx)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CRichEditEx message handlers
//
// Following set char formatting over the current selection. If empty, should
// apply to insertion point format.
// SetCharBold,Italic,Symbol toggle the attribute (set if all bold/italic/symbol).
//
void CRichEditEx::ToggleCharBold()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetSelectionCharFormat(cf);
	
	// if sel not consistently bold, set it all to bold
	if (!(cf.dwMask & CFM_BOLD) || !(cf.dwEffects & CFE_BOLD))
		cf.dwEffects = CFE_BOLD;
	else	// turn off bold
		cf.dwEffects = 0;

	cf.dwMask = CFM_BOLD;
	SetSelectionCharFormat(cf);
}

void CRichEditEx::ToggleCharItalic()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetSelectionCharFormat(cf);
	
	// if sel not consistently italic, set it all to italic
	if (!(cf.dwMask & CFM_ITALIC) || !(cf.dwEffects & CFE_ITALIC))
		cf.dwEffects = CFE_ITALIC;
	else // turn off italic
		cf.dwEffects = 0;

	cf.dwMask = CFM_ITALIC;
	SetSelectionCharFormat(cf);

}

void CRichEditEx::ToggleCharSymbol()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetSelectionCharFormat(cf);

	// if sel face not consistently "Symbol" change it to symbol
	if (!(cf.dwMask & CFM_FACE) || !(lstrcmp(cf.szFaceName, "Symbol") == 0)) {
		::lstrcpy(cf.szFaceName, "Symbol");

	} else {
		// restore default face (from default charformat, which we never change.)
		CHARFORMAT cfDefault;
		cfDefault.cbSize = sizeof(CHARFORMAT);
		GetDefaultCharFormat(cfDefault);
		::lstrcpy(cf.szFaceName, cfDefault.szFaceName); 
	}

	cf.dwMask = CFM_FACE;
	SetSelectionCharFormat(cf);

}

void CRichEditEx::GetRichEditText(CString & strEq)
{	//We are getting the text from the rich edit and adding a dollar sign
	//before any greek letters.  So help system can interpret

	CHARRANGE crOldSel;			// saves current selection
	GetSel(crOldSel);
	HideSelection(TRUE, FALSE);	// temporarily turn off selection highlighting
	
	CString strSel;
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);

	strEq.Empty();			// loop appends, so must clear any chars in arg
	int len = GetTextLength();
	for (int i=0; i<len; i++){
		SetSel(i, i+1);
		GetSelectionCharFormat(cf);
		if ((cf.dwMask & CFM_FACE) && (lstrcmp(cf.szFaceName, "Symbol") == 0)) 
			strSel = "$" + GetSelText();
		else
			strSel = GetSelText();
		strEq += strSel;
	}

	// SetSel(len, len);//Move selection to end of line. // ?? why - AW
	SetSel(crOldSel);				// restores old selection
	HideSelection(FALSE, FALSE);	// restores selection highlighting
}

void CRichEditEx::SetRichEditText(CString &strEq)
{
	CString strRest(strEq);  // remaining text from strEq to scan for Greek

	SetSel(0, -1);
	SetCharPlain();			 // ensure reset format to plain text
	while (strRest.Find("$") != -1) 
	{
		int grkPos = strRest.Find('$');		//find greek letter
		int strlen = strRest.GetLength();
		if (grkPos >= (strlen-1))//if at last position, break
			break;
		if (grkPos != -1) 
		{// insert text before greek letter
			CString text = strRest.Left(grkPos);
			if (!text.IsEmpty()) 
				ReplaceSel(text);

			CString c = strRest[grkPos +1]; //make sure letter follows $
			if ( ((c >= "a") && ( c<="z"))||((c >= "A") && ( c<="Z")) 
				 || (c >= "\306") || (c<="\310")) // empty set, intersection or union
			{
				//SetCharSymbol();
				//ReplaceSel(c);	//insert greek letter
				InsertGreekText(c);
				SetCharPlain();			// go back to plain text
			}
			else
			{
				CString tempStr = "$" + c; //insert actual $ and what follows
				ReplaceSel(tempStr);
			}
			strRest = strRest.Mid(grkPos+2);
		}

	}
	ReplaceSel(strRest);
}

// SetCharSymbol forces greek text

void CRichEditEx::SetCharSymbol()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);		
	cf.dwMask =  CFM_FACE;					// Change Face to symbol
	::lstrcpy(cf.szFaceName, "Symbol");
	SetSelectionCharFormat(cf);				// Set the selection format
}

// SetCharPlain forces plain text

void CRichEditEx::SetCharPlain()
{
	// undo char format effects
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	cf.dwEffects = 0;
	// restore default face (from default charformat, which we never change.)
	CHARFORMAT cfDefault;
	cfDefault.cbSize = sizeof(CHARFORMAT);
	GetDefaultCharFormat(cfDefault);
	::lstrcpy(cf.szFaceName, cfDefault.szFaceName); 
	
	cf.dwMask = CFM_ITALIC | CFM_BOLD | CFM_FACE ;
	SetSelectionCharFormat(cf);
}

void CRichEditEx::InsertGreekText(CString strText)
{
	// Simple method consisting of SetCharSymbol to set format at insertion point followed
	// by ReplaceSel does not display Greek symbols in the richedit control on Windows 
	// 2000 and Me, even though the Greek format attributes are correctly retreived on 
	// those character positions in the control. Following inserts the text first, 
	// selects it, then changes format.
	long nStart, nEnd;			// initial selection range (maybe just inesertion pt)
	GetSel(nStart, nEnd);		// save initial selection 
	ReplaceSel(strText);		// Replace sel contents w/text
	int nEndText = nStart + strText.GetLength(); // endpoint of newly inserted text
	SetSel(nStart, nEndText);	// Select newly inserted text
	SetCharSymbol();			// change sel format to Symbol
	SetSel(nEndText, nEndText);	// leave sel at insertion pt after inserted text
}

/////////////////////////////////////////////////////////////////////////////
// CHintRichEdit

CHintRichEdit::CHintRichEdit()
{
	m_bOnHyperText = FALSE;
}

CHintRichEdit::~CHintRichEdit()
{
	// free up hyperlink list 
	ClearLinks();
}

void CHintRichEdit::ClearLinks()
{
	while (!m_links.IsEmpty())
		m_links.RemoveHead()->Delete();
}

BEGIN_MESSAGE_MAP(CHintRichEdit, CRichEditCtrl)
	//{{AFX_MSG_MAP(CHintRichEdit)
	ON_WM_MOUSEACTIVATE()
	ON_WM_MOUSEMOVE()
	ON_WM_LBUTTONDOWN()
	ON_WM_SETCURSOR()
	ON_WM_SETFOCUS()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHintRichEdit message handlers

// Windows sends this message on mouse clicks in an inactive window.
// Normally seems to be passed to DefWindowProc which sends it to the parent
// (a sort of notification). Looks like MFC catches it and uses to activate
// the containing view, which is important for command UI enabling.
// But this is not happening with our richedit. So we trap it and explicitly 
// forward it to the parent here. Urgh.
// Turns out KB article Q166213 documents this as a bug in CRichEditCtrl and
// gives the following code:
int CHintRichEdit::OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message) 
{
	const MSG* pMsg = GetCurrentMessage();
	GetParent()->SendMessage(WM_MOUSEACTIVATE, pMsg->wParam, pMsg->lParam);
		
	return CRichEditCtrl::OnMouseActivate(pDesktopWnd, nHitTest, message);
}

void CHintRichEdit::OnMouseMove(UINT nFlags, CPoint point) 
{
	if (HyperAt(point) !=  NULL)
		m_bOnHyperText = TRUE;
	else
		m_bOnHyperText = FALSE;
	
	
	CRichEditCtrl::OnMouseMove(nFlags, point);
}

void CHintRichEdit::OnLButtonDown(UINT nFlags, CPoint point) 
{
	CHyperLnk* pLnk = HyperAt(point);
	if (pLnk != NULL){
		HCURSOR hCursor = AfxGetApp()->LoadStandardCursor(IDC_ARROW);
		::SetCursor(hCursor);
		m_bOnHyperText = FALSE;
		pLnk->HyperActivate(point, ((CView*)GetParent()));
		return;
	}
	
	//eat this so caret doesn't appear
	//CRichEditCtrl::OnLButtonDown(nFlags, point);
}

void CHintRichEdit::ClearFormat()
{//clear color, underline, greek alphabet
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetSelectionCharFormat(cf);

	CHARFORMAT cfDefault;
	cfDefault.cbSize = sizeof(CHARFORMAT);
	GetDefaultCharFormat(cfDefault);
	::lstrcpy(cf.szFaceName, cfDefault.szFaceName); 

	cf.dwMask = CFM_COLOR | CFM_UNDERLINE | CFM_FACE;
	cf.dwEffects = CFE_AUTOCOLOR;
	SetSelectionCharFormat(cf);
}

void CHintRichEdit::SetCharUnderline()
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetSelectionCharFormat(cf);
	
	// if sel not consistently underlined, underline it all
	if (!(cf.dwMask & CFM_UNDERLINE) || !(cf.dwEffects & CFE_UNDERLINE))
		cf.dwEffects = CFE_UNDERLINE;
	else	// turn off underline
		cf.dwEffects = 0;

	cf.dwMask = CFM_UNDERLINE;
	SetSelectionCharFormat(cf);
}

void CHintRichEdit::SetCharLinkColor(char tag)
{
	CHARFORMAT cf;
	cf.cbSize = sizeof(CHARFORMAT);
	GetSelectionCharFormat(cf);
	
	if ( ((cf.dwMask & CFM_COLOR)&&(cf.dwEffects & CFE_AUTOCOLOR)) ||
		!(cf.dwMask & CFM_COLOR) ){
		cf.dwEffects = 0;
		cf.dwMask = cf.dwMask | CFM_COLOR;
		if (tag == 'd')	// "definition" i.e. popup
			cf.crTextColor = RGB(0, 0, 128); // navy blue
		// else if (tag == 'h') // helpsys call
		//	cf.crTextColor = RGB(0, 128, 128); // "teal"
		else // default: all others as hyperlink jumps
			cf.crTextColor = RGB(128, 0, 128); // purple
	}
	else	// turn off color (autocolor ignores crTextColor and uses system color)
		cf.dwEffects = cf.dwEffects | CFE_AUTOCOLOR;
	cf.dwMask = CFM_COLOR;
	SetSelectionCharFormat(cf);
}

void CHintRichEdit::SetFormat(char tag)
{
	if (tag == 'b')//we want bold
	{
		ToggleCharBold();
	}
	else if (tag == 'i')//we want italics
	{
		ToggleCharItalic();
	}
	else{//we have a link
		SetCharUnderline();
		SetCharLinkColor(tag);//other link is p (I think)
	}
}

CHyperLnk* CHintRichEdit::HyperAt(CPoint point)
{
	POINT pt;
	pt.x = point.x;
	pt.y = point.y;
	LPPOINT lppoint = &pt;
	LPARAM nPos = (LPARAM)lppoint;
//Get closest char to point
	DWORD pos = SendMessage(EM_CHARFROMPOS , 0, nPos);
	LONG poschar = LOWORD(pos);
//selection that character
	SetSel(poschar, poschar);
	CHARFORMAT cf;
//get its format	
	GetSelectionCharFormat(cf);
	if ( !(cf.dwMask & CFM_UNDERLINE) || !(cf.dwMask & CFM_COLOR)
		|| !(cf.dwEffects & CFE_UNDERLINE) || (cf.dwEffects & CFE_AUTOCOLOR))//!CFE_AUTOCOLOR) )
		return NULL;//if not underlined and in color, not a link
	
	return GetLinkFromPos(poschar);

}

CHyperLnk* CHintRichEdit::GetLinkFromPos(int poschar)
{
	POSITION pos = m_links.GetTailPosition();
	while (pos != NULL)
	{
		CHyperLnk* pLnk = m_links.GetPrev(pos);
		if ((pLnk->m_posBeg <= poschar) && (pLnk->m_posEnd >= poschar))
			return pLnk;
	}
	return NULL;
}

BOOL CHintRichEdit::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message) 
{
	
	if (m_bOnHyperText){//if over hypertext, load hand cursor
		HCURSOR hCursor = AfxGetApp()->LoadCursor(IDC_LINK);
		::SetCursor(hCursor);	
		return TRUE;
	}
	else{//owtherwise, load arrow cursor
		HCURSOR hCursor = AfxGetApp()->LoadStandardCursor(IDC_ARROW);
		::SetCursor(hCursor);	
		return TRUE;
	}
	//never gets here
	return CRichEditCtrl::OnSetCursor(pWnd, nHitTest, message);

}


void CHintRichEdit::OnSetFocus(CWnd* pOldWnd) 
{//eat message so no caret
//	CRichEditCtrl::OnSetFocus(pOldWnd);
}

void CHintRichEdit::SetParaIndents(int lIndent, int rIndent )
{//indents are in twips (1/1440 of an inch)
	PARAFORMAT pf;
	pf.cbSize = sizeof(PARAFORMAT);
	GetParaFormat(pf);
	
	pf.dwMask = pf.dwMask | PFM_STARTINDENT | PFM_RIGHTINDENT;
	pf.dxStartIndent = lIndent;
	pf.dxRightIndent = rIndent;
	SetParaFormat(pf);
}

void CHintRichEdit::SetRichEditText(CString & strHint, int nMargin)
{
	SetSel(0, -1);
	ClearFormat();//Need to force a clear format here.  In case character at zero
	//has the color and underline format or greek letter format. If so, that would become 
	//the current format and will invert the format we want when we show greek letters
	//or links
	ClearLinks();	// reset link list to empty
	strHint.Replace("\\n", "\n");
	while (strHint.FindOneOf("{$") != -1)
	{
		int grkPos = strHint.Find('$');		//find greek letter
		int lnkPos = strHint.Find('{');		//find first part of link

		//Which came first? the chicken or the egg (greek or link?)
		if ((grkPos != -1) && ((lnkPos == -1) || (grkPos < lnkPos)))
		{// insert text before greek letter
			CString text = strHint.Left(grkPos);
			if (!text.IsEmpty())
				ReplaceSel(text);

			CString c = strHint[grkPos +1]; //make sure letter follows $
			if ( ((c >= "a") && ( c<="z"))||((c >= "A") && ( c<="Z")) 
				  || (c >= "\306") || (c<="\310")) // empty set, intersection or union
			{
				ToggleCharSymbol();
				ReplaceSel(c);	//insert greek letter
				ToggleCharSymbol();
			}
			else
			{
				CString tempStr = "$" + c; //insert actual $ and what follows
				ReplaceSel(tempStr);
			}
			strHint = strHint.Mid(grkPos+2);

		}
		else if (lnkPos != -1)
		{// insert text before the link
			CString text = strHint.Left(lnkPos);
			if (!text.IsEmpty())
				ReplaceSel(text);
			CString restofStr = strHint.Mid(lnkPos);
			strHint = ParseOutFmtTxt(restofStr);
		}

	}
	CString tempStr = strHint;
	strHint = tempStr;
	// replace any text left over
	if (!strHint.IsEmpty())
		ReplaceSel(strHint);
	//for paragraph formatting to work, need to select everything
	//then set the format.  My function takes leftIndent, rightIndent
	SetSel(0, -1);
	SetParaIndents(nMargin, nMargin);
	//Set format back to zero so hint isn't highlighted.
	SetSel(0, 0);

	// DumpLinkPositions(); // show for debugging
}

CString CHintRichEdit::ParseOutFmtTxt(CString str)
{
	int posBeg = str.Find('{');		
	int posEnd = str.Find('}');
	int len = posEnd - posBeg;
	CString lnkPrt1 = str.Mid(posBeg, len);//find first part of formatted text
											//link has two parts
	CRect rect;
	LONG selPos1, selPos2;

	GetSel(selPos1, selPos2);
	rect.TopLeft() = GetCharPos(selPos2); // assign first part of 
															// link rect	
	//find link tag
	char tag = lnkPrt1[2];			// "0{" "1\" "2tag"
	SetFormat(tag);	// change format to underline 
											// blue or purple color for links/jumps
					//or could change to bold/italic
	//insert link text
	int len1 = lnkPrt1.GetLength();
	CString lnkTxt = lnkPrt1.Mid(4, len1-4);
	
	if (!lnkTxt.IsEmpty())
		ReplaceSel(lnkTxt);

	SetFormat(tag);	//set format back


	CString strRest = str.Mid(posEnd + 1);
	if ((tag == 'b') || (tag == 'i'))
		return strRest;
	//if link, find second part
	posBeg = strRest.Find('{');
	posEnd = strRest.Find('}');
	len = posEnd - posBeg;
	CString lnkPrt2 = strRest.Mid(posBeg, len);
	int len2 = lnkPrt2.GetLength();
	CString lnkId = lnkPrt2.Mid(4, len2 - 4); // "{0" "/1" "v2" "space3"
	//if link, create 
	CHyperLnk* pLnk = new CHyperLnk();
	pLnk->m_posBeg = selPos1; 
	GetSel(selPos1, selPos2); 
	pLnk->m_posEnd = selPos1;
	rect.BottomRight() = GetCharPos(selPos2);// assign second part 
																// of link rect
	pLnk->m_strLink = lnkId;
	pLnk->m_position = rect;
	pLnk->m_strName = lnkTxt;
	pLnk->SetLinkType(tag);
	
	m_links.AddTail(pLnk);

	str = strRest.Mid(posEnd + 1);
	return str;

}

// return first link whose text begins w/given prefix (case insensitive), NULL if none
CHyperLnk* CHintRichEdit::FindLink(LPCTSTR pszPrefix)
{
	POSITION pos = m_links.GetHeadPosition();
	while (pos != NULL) {
		CHyperLnk* pLnk = m_links.GetNext(pos);
		if (_strnicmp(pszPrefix, pLnk->m_strName, strlen(pszPrefix)) == 0)
			return pLnk;
	}
	return NULL;
}

#if _DEBUG
void CHintRichEdit::DumpLinkPositions()
{
	// show plain text str w/position indices for comparison
	CString strPlainText;
	GetWindowText(strPlainText);
	TRACE("Hint text:\n");
	for (unsigned int i = 0; i < strlen(strPlainText); i++) {
		char ch = strPlainText.GetAt(i);
		TRACE("%2d:%c ", i, isprint(ch) ? ch : '#');
		if ((i+1) % 20 == 0) TRACE("\n"); 
	}
	TRACE("\nHyperlinks:\n");
	POSITION pos = m_links.GetHeadPosition();
	while (pos != NULL) {
		CHyperLnk* pLnk = m_links.GetNext(pos);
		TRACE("%s: posBeg=%d posEnd=%d\n", pLnk->m_strName, pLnk->m_posBeg, pLnk->m_posEnd);
	}
}
#endif _DEBUG

