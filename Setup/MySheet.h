#if !defined(AFX_MYSHEET_H__D8922942_43C9_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_MYSHEET_H__D8922942_43C9_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// MySheet.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CMySheet

class CMySheet : public CPropertySheet
{
	DECLARE_DYNAMIC(CMySheet)

// Construction
public:
	CMySheet(UINT nIDCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);
	CMySheet(LPCTSTR pszCaption, CWnd* pParentWnd = NULL, UINT iSelectPage = 0);


// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMySheet)
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMySheet();

	// Generated message map functions
protected:
	//{{AFX_MSG(CMySheet)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
public:
	afx_msg void OnCancel();
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MYSHEET_H__D8922942_43C9_11D1_A09F_0000C0086DCF__INCLUDED_)
