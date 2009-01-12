#if !defined(AFX_EXP4DLG_H__DF5D05C2_60CE_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_EXP4DLG_H__DF5D05C2_60CE_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// Exp4Dlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CExp4Dlg dialog
#include "TemplateDlg.h"

class CFBDDoc;


class CExp4Dlg : public CTemplateDlg
{
// Construction
public:
	void ResetTemplate();

	CWnd* m_pParent;
	CFBDDoc* m_pDoc;
	CExp4Dlg(CFBDDoc* pDoc = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CExp4Dlg)
	enum { IDD = IDD_EXPLAIN_FOUR };
	private:
	CStatic	m_stcThen;
	CStatic	m_stcIF;
	CLogCombo	m_cboDueTo;
	CLogCombo	m_cboAND2;
	CLogCombo	m_cboAND1;
	CLogCombo	m_cboIF;
	CStatic	m_stcAND2;
	CStatic	m_stcAND1;
	//}}AFX_DATA

// operations
	virtual void InsertCtrlStrs();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CExp4Dlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CExp4Dlg)
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	afx_msg void OnSubmit();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EXP4DLG_H__DF5D05C2_60CE_11D1_A09F_0000C0086DCF__INCLUDED_)
