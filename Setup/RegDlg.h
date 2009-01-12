#if !defined(AFX_REGDLG_H__B9709281_49F6_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_REGDLG_H__B9709281_49F6_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// RegDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CRegDlg dialog
class CMySheet;

class CRegDlg : public CDialog
{
// Construction
public:
	CMySheet* m_pSht;
	CRegDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CRegDlg)
	enum { IDD = IDD_REG_DLG };
	CStatic	m_txt;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRegDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CRegDlg)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_REGDLG_H__B9709281_49F6_11D1_A09F_0000C0086DCF__INCLUDED_)
