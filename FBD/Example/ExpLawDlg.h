#if !defined(AFX_EXPLAWDLG_H__2D9FE761_FC5C_11D1_A6D7_0000C0086DCF__INCLUDED_)
#define AFX_EXPLAWDLG_H__2D9FE761_FC5C_11D1_A6D7_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ExpLawDlg.h : header file
//
#include "EQEdit.h"

/////////////////////////////////////////////////////////////////////////////
// CExpLawDlg dialog

class CExpLawDlg : public CDialog
{
// Construction
public:
	CExpLawDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CExpLawDlg)
	enum { IDD = IDD_EXPLAIN_LAW };
	//}}AFX_DATA


	int m_nId;
	CHintRichEdit m_editLaw;

//helper function
protected:
	
	void UpdateExplanation();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CExpLawDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CExpLawDlg)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnMove(int x, int y);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#define ID_EXP_NEWTONSFIRST		0
#define ID_EXP_NEWTONSSECOND	1
#define ID_EXP_KINEMATICS		2
#define ID_EXP_ENERGY			3
#define ID_EXP_NETWORK			4

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EXPLAWDLG_H__2D9FE761_FC5C_11D1_A6D7_0000C0086DCF__INCLUDED_)
