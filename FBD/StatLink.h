////////////////////////////////////////////////////////////////
// MSDN Magazine -- October 2001
// If this code works, it was written by Paul DiLascia.
// If not, I don't know who wrote it.
// Compiles with Visual C++ 6.0 for Windows 98 and probably Windows 2000 too.
// Set tabsize = 3 in your editor.
//
#pragma once

//////////////////
// Simple text hyperlink derived from CString
//
class CHyperlink : public CString {
public:
	CHyperlink(LPCTSTR lpLink = NULL) : CString(lpLink) { }
	~CHyperlink() { }
	const CHyperlink& operator=(LPCTSTR lpsz) {
		CString::operator=(lpsz);
		return *this;
	}
	operator LPCTSTR() {
		return CString::operator LPCTSTR(); 
	}
	HINSTANCE Navigate() {
		return  IsEmpty() ? NULL :
			ShellExecute(0, _T("open"), *this, 0, 0, SW_SHOWNORMAL);
	}
};

class CStaticLink : public CStatic {
public:
	DECLARE_DYNAMIC(CStaticLink)
	CStaticLink(LPCTSTR lpText = NULL, BOOL bDeleteOnDestroy=FALSE);
	~CStaticLink() { }

	// Use this if you want to subclass and also set different URL
	BOOL SubclassDlgItem(UINT nID, CWnd* pParent, LPCTSTR lpszLink=NULL) {
		m_link = lpszLink;
		return CStatic::SubclassDlgItem(nID, pParent);
	}

	void SetNoLink(BOOL bNoLink) {
		m_bNoLink = bNoLink;
	}

	// Hyperlink contains URL/filename. If NULL, I will use the window text.
	// (GetWindowText) to get the target.
	CHyperlink	m_link;
	COLORREF		m_color;

	// Default colors you can change
	// These are global, so they're the same for all links.
	static COLORREF g_colorUnvisited;
	static COLORREF g_colorVisited;

	// Cursor used when mouse is on a link--you can set, or
	// it will default to the standard hand with pointing finger.
	// This is global, so it's the same for all links.
	static HCURSOR	 g_hCursorLink;

protected:
	CFont			m_font;					// underline font for text control
	BOOL			m_bDeleteOnDestroy;	// delete object when window destroyed?
	BOOL			m_bNoLink;

	BOOL TryNavigate();
	virtual void PostNcDestroy();

	// message handlers
	DECLARE_MESSAGE_MAP()
	afx_msg UINT	OnNcHitTest(CPoint point);
	afx_msg HBRUSH CtlColor(CDC* pDC, UINT nCtlColor);
	afx_msg void	OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg BOOL	OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message);
};
