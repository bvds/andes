#if !defined(AFX_CHKSPACEDLG_H__84D73CE1_43F2_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_CHKSPACEDLG_H__84D73CE1_43F2_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ChkSpaceDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CChkSpaceDlg dialog
class CMySheet;

class CChkSpaceDlg : public CDialog
{
// Construction
public:
	CMySheet* m_pSht;
	CChkSpaceDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CChkSpaceDlg)
	enum { IDD = IDD_CHECKSPACE_DLG };
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChkSpaceDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CChkSpaceDlg)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CHKSPACEDLG_H__84D73CE1_43F2_11D1_A09F_0000C0086DCF__INCLUDED_)
