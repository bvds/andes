#if !defined(AFX_TRACEDLG_H__7B834F40_1707_11D4_807C_0000C546451F__INCLUDED_)
#define AFX_TRACEDLG_H__7B834F40_1707_11D4_807C_0000C546451F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// TraceDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CTraceDlg dialog

class CTraceDlg : public CDialog
{
// Construction
public:
	void AddMsg(LPCTSTR pszMsg);
	CTraceDlg(CWnd* pParent = NULL);   // standard constructor

	BOOL Create(CWnd* pParent=NULL)
	{ return CDialog::Create(IDD, pParent); }

// Dialog Data
	//{{AFX_DATA(CTraceDlg)
	enum { IDD = IDD_TRACE };
	CRichEditCtrl	m_edit;
	//}}AFX_DATA

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTraceDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CTraceDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TRACEDLG_H__7B834F40_1707_11D4_807C_0000C546451F__INCLUDED_)
