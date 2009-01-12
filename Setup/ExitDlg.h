#if !defined(AFX_EXITDLG_H__D8922941_43C9_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_EXITDLG_H__D8922941_43C9_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ExitDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CExitDlg dialog

class CExitDlg : public CDialog
{
// Construction
public:
	CExitDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CExitDlg)
	enum { IDD = IDD_EXIT_DLG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CExitDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CExitDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EXITDLG_H__D8922941_43C9_11D1_A09F_0000C0086DCF__INCLUDED_)
