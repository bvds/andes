#if !defined(AFX_CLEANUP_H__4DCA5F01_4BBF_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_CLEANUP_H__4DCA5F01_4BBF_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// Cleanup.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CCleanup dialog

class CCleanup : public CDialog
{
// Construction

public:
	CCleanup(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CCleanup)
	enum { IDD = IDD_CLEANUP_DLG };
	CStatic	m_stcText;
	CString	m_strText;
	//}}AFX_DATA

	BOOL m_bRestart;
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCleanup)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CCleanup)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CLEANUP_H__4DCA5F01_4BBF_11D1_A09F_0000C0086DCF__INCLUDED_)
