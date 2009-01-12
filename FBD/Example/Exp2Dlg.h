#if !defined(AFX_EXP2DLG_H__DF5D05C1_60CE_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_EXP2DLG_H__DF5D05C1_60CE_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// Exp2Dlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CExp2Dlg dialog
#include "TemplateDlg.h"

class CFBDDoc;


class CExp2Dlg : public CTemplateDlg
{
// Construction
public:
	void ResetTemplate();

	CWnd* m_pParent;
	CFBDDoc* m_pDoc;

	CExp2Dlg(CFBDDoc* pDoc = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CExp2Dlg)
	enum { IDD = IDD_EXPLAIN_TWO };
	private:
	CStatic	m_stcAND1;
	CStatic	m_stcThen;
	CStatic	m_stcIF;
	CLogCombo	m_cboAND1;
	CLogCombo	m_cboIF;
	//}}AFX_DATA

	virtual void InsertCtrlStrs();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CExp2Dlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

// Implementation
protected:


	// Generated message map functions
	//{{AFX_MSG(CExp2Dlg)
	afx_msg void OnSubmit();
	afx_msg void OnShowWindow(BOOL bShow, UINT nStatus);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_EXP2DLG_H__DF5D05C1_60CE_11D1_A09F_0000C0086DCF__INCLUDED_)
