////////////////////////////////////////////////////////////////
// MSDN Magazine -- October 2001
// If this code works, it was written by Paul DiLascia.
// If not, I don't know who wrote it.
// Compiles with Visual C++ 6.0 for Windows 98 and probably Windows 2000 too.
// Set tabsize = 3 in your editor.
//
#include "StdAfx.h"
#include "StatLink.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// hyperlink colors; you can change globally or per control
COLORREF CStaticLink::g_colorUnvisited = RGB(0,0,255);		 // blue
COLORREF CStaticLink::g_colorVisited   = RGB(128,0,128);		 // purple

// finger-pointing hyperlink cursor
HCURSOR CStaticLink::g_hCursorLink = NULL;

IMPLEMENT_DYNAMIC(CStaticLink, CStatic)
BEGIN_MESSAGE_MAP(CStaticLink, CStatic)
	ON_WM_NCHITTEST()
	ON_WM_CTLCOLOR_REFLECT()
	ON_WM_LBUTTONDOWN()
	ON_WM_SETCURSOR()
END_MESSAGE_MAP()

///////////////////
// Constructor sets default colors = blue/purple.
//
CStaticLink::CStaticLink(LPCTSTR lpText, BOOL bDeleteOnDestroy)
{
	m_link = lpText;								// link text (NULL ==> window text)
	m_color = g_colorUnvisited;				// not visited yet
	m_bDeleteOnDestroy = bDeleteOnDestroy;	// delete object with window?
	m_bNoLink = FALSE;							// assume I have a link
}

//////////////////
// Normally,	a static control does not get mouse events unless it has
// SS_NOTIFY. This achieves the same effect as SS_NOTIFY, but it's fewer
// lines of code and more reliable than turning on SS_NOTIFY in OnCtlColor
// because Windows doesn't send WM_CTLCOLOR to bitmap static controls.
//
UINT CStaticLink::OnNcHitTest(CPoint point)
{
	return HTCLIENT;
}

//////////////////
// Handle reflected WM_CTLCOLOR to set custom control color.
// For a text control, use visited/unvisited colors and underline font.
// For non-text controls, do nothing. Also ensures SS_NOTIFY is on.
//
HBRUSH CStaticLink::CtlColor(CDC* pDC, UINT nCtlColor)
{
	ASSERT(nCtlColor == CTLCOLOR_STATIC);
	DWORD dwStyle = GetStyle();
	HBRUSH hbr = NULL;
	if (!m_bNoLink && ((dwStyle & 0xFF) <= SS_RIGHT)) {

		// this is a text control: set up font and colors
		if (!(HFONT)m_font) {
			// first time init: create font
			LOGFONT lf;
			GetFont()->GetObject(sizeof(lf), &lf);
			lf.lfUnderline = TRUE;
			m_font.CreateFontIndirect(&lf);
		}

		// use underline font and visited/unvisited colors
		pDC->SelectObject(&m_font);
		pDC->SetTextColor(m_color);
		pDC->SetBkMode(TRANSPARENT);

		// return hollow brush to preserve parent background color
		hbr = (HBRUSH)::GetStockObject(HOLLOW_BRUSH);
	}
	return hbr;
}

/////////////////
// Handle mouse click: navigate link
//
void CStaticLink::OnLButtonDown(UINT nFlags, CPoint point)
{
	if (!m_bNoLink && !TryNavigate()) {
		// try window text
		GetWindowText(m_link);
		if (!TryNavigate()) {
			// try resource string
			m_link.LoadString(GetDlgCtrlID());
			if (!TryNavigate()) {
				// unable to navigate link!
				MessageBeep(0);
				TRACE(_T("*** CStaticLink: can't navigate %s!\n"), (LPCTSTR)m_link);
				m_bNoLink = TRUE;
			}
		}
	}
}
			
//////////////////
// Try to navigate the link
//
BOOL CStaticLink::TryNavigate()
{
	HINSTANCE h = m_link.Navigate();
	if ((UINT)h > 32) {						 // success!
		m_color = g_colorVisited;			 // change color
		Invalidate();							 // repaint
		return TRUE;
	}
	return FALSE;
}

//////////////////
// Set "hand" cursor to cue user that this is a link. If app has not set
// g_hCursorLink, then try to get the cursor from winhlp32.exe,
// resource 106, which is a pointing finger. This is a bit of a kludge,
// but it works.
//
BOOL CStaticLink::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
	if (!m_bNoLink) {
		if (g_hCursorLink == NULL) {
			static bTriedOnce = FALSE;
			if (!bTriedOnce) {
				HMODULE hModule = LoadLibrary(_T("winhlp32.exe"));
				if (hModule) {
					g_hCursorLink =
						CopyCursor(::LoadCursor(hModule, MAKEINTRESOURCE(106)));
				}
				FreeLibrary(hModule);
				bTriedOnce = TRUE;
			}
		}
		if (g_hCursorLink) {
			::SetCursor(g_hCursorLink);
			return TRUE;
		}
	}
	return FALSE;
}

//////////////////
// Normally, a control class is not destoyed when the window is;
// however, CPixieDlg creates static controls with "new" instead of
// as class members, so it's convenient to allow the option of destroying
// object with window. In applications where you want the object to be
// destoyed along with the window, you can call constructor with
// bDeleteOnDestroy=TRUE.
//
void CStaticLink::PostNcDestroy()
{
	if (m_bDeleteOnDestroy)
		delete this;
}
