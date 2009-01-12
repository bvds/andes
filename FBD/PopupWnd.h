#if !defined(AFX_POPUPWND_H__BD6B08E1_BD8A_11D1_A6D7_0000C0086DCF__INCLUDED_)
#define AFX_POPUPWND_H__BD6B08E1_BD8A_11D1_A6D7_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PopupWnd.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPopupWnd window
#define SHADOW_WIDTH	16
#define SHADOW_HEIGHT	16
#define SHADOW_OFFSET	16

#define WM_POPUPDEF		WM_USER + 1
#define WM_KILLPOPUP	WM_USER + 2

class CPopupWnd : public CWnd, public IEventHandler
{
// Construction
public:
	CPopupWnd();

// Attributes
public:

// Operations
public:

	BOOL Create(CWnd* pParentWnd = NULL, CPoint pos = (0,0));

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPopupWnd)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

	// Log/Demo support
	virtual BOOL DispatchEvent(EventID nEvent, LPCTSTR parms);
//	virtual void PointToObject(LPCTSTR pszObjID);


// Implementation
public:
	CString m_strDef;
	virtual ~CPopupWnd();

	// Generated message map functions
protected:
	//{{AFX_MSG(CPopupWnd)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnPaint();
	//}}AFX_MSG
	afx_msg LRESULT OnKillPopup(WPARAM msg, LPARAM hwnd);
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_POPUPWND_H__BD6B08E1_BD8A_11D1_A6D7_0000C0086DCF__INCLUDED_)
