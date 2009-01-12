#if !defined(AFX_SOLVEVARDLG_H__891A8400_EB90_11D2_B260_0000C5465DC1__INCLUDED_)
#define AFX_SOLVEVARDLG_H__891A8400_EB90_11D2_B260_0000C5465DC1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// SolveVarDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CSolveVarDlg dialog
#include "RichCombo.h"

class CSolveVarDlg : public CLogDialog
{
// Construction
public:
	CSolveVarDlg(CStringList* pStrList, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CSolveVarDlg)
	enum { IDD = IDD_SOLVEFOR };
	CRichCombo	m_cboVarList;
	//}}AFX_DATA
	CStringList*	m_pStrList;
	CString			m_strVar;


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CSolveVarDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CSolveVarDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SOLVEVARDLG_H__891A8400_EB90_11D2_B260_0000C5465DC1__INCLUDED_)
