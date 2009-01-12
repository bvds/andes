#if !defined(AFX_SYMBOLMENU_H__0187AC23_AD07_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_SYMBOLMENU_H__0187AC23_AD07_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// SymbolMenu.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CSymbolMenu dialog

class CSymbolMenu : public CDialog
{
// Construction
public:
	void EnableButtons(BOOL bEnable);
	CToolBarCtrl m_TBar;
	CSymbolMenu(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CSymbolMenu)
	enum { IDD = IDD_GREEKBAR };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSymbolMenu)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CSymbolMenu)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	afx_msg void OnToolTip(UINT id, NMHDR * pTTTStruct, LRESULT * pResult );
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SYMBOLMENU_H__0187AC23_AD07_11D1_A09F_0000C0086DCF__INCLUDED_)
