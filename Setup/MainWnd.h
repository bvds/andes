#if !defined(AFX_MAINWND_H__A2E112A1_43C6_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_MAINWND_H__A2E112A1_43C6_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// MainWnd.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMainWnd frame

class CMainWnd : public CFrameWnd
{
	DECLARE_DYNCREATE(CMainWnd)
	CMainWnd();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainWnd)
	//}}AFX_VIRTUAL

// Implementation
protected:
	CString m_text; 
    int m_nHeight;
	CBitmap m_bmpAndes;
    BOOL m_bBold;
    BOOL m_bItalic;
	void DoDrawText (CDC* pDC, CRect* pRect);
	void DoGradientFill (CDC* pDC, CRect* pRect);

	virtual ~CMainWnd();

	// Generated message map functions
	//{{AFX_MSG(CMainWnd)
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MAINWND_H__A2E112A1_43C6_11D1_A09F_0000C0086DCF__INCLUDED_)
