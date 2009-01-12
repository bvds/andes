#if !defined(AFX_PROPERTYDLG_H__E4DC79E1_06B4_11D2_B9ED_0000C5465DC1__INCLUDED_)
#define AFX_PROPERTYDLG_H__E4DC79E1_06B4_11D2_B9ED_0000C5465DC1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PropertyDlg.h : header file
//



/////////////////////////////////////////////////////////////////////////////
// CPropertyDlg dialog
#include "DrawObjDlg.h"

class CPropertyDlg : public CDrawObjDlg
{
// Construction
public:
	CPropertyDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	DECLARE_CTL_TBL()

	CString m_strProp;

// Dialog Data
	//{{AFX_DATA(CPropertyDlg)
	enum { IDD = IDD_PROPERTY };
	CStatic	m_stcValue;
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CLogCombo	m_cboTime;
	CLogCombo	m_cboObject;
	CStatic	m_stcHeader;
	CStatic	m_stcPrep2;
	CStatic	m_stcPrep1;
	//}}AFX_DATA

	CString m_strPrep1;
	CString m_strPrep2;

// Helper functions
	void BuildProperty();
	void UpdateProperty();



// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPropertyDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CPropertyDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSelchangeBody();
	afx_msg void OnSelchangeTime();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};



//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PROPERTYDLG_H__E4DC79E1_06B4_11D2_B9ED_0000C5465DC1__INCLUDED_)
