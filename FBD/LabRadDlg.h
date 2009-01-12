#if !defined(AFX_LABRADDLG_H__C2FF9B82_A84B_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_LABRADDLG_H__C2FF9B82_A84B_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// LabRadDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CRadiusDlg dialog
#include "DrawObjDlg.h"
#include "EQEdit.h"			// for rich edit label of angle

class CRadiusDlg : public CDrawObjDlg
{
// Construction
public:
	CRadiusDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	DECLARE_CTL_TBL()

	virtual CLabelRichEdit* GetLabelCtrl();

// Dialog Data
	//{{AFX_DATA(CRadiusDlg)
	enum { IDD = IDD_LABELRADIUS };
	CStatic	m_stcRad;
	CStatic	m_stcLet;
	CLogBtn	m_btnOK;
	CLogBtn	m_btnCancel;
	CLabelRichEdit	m_editName;
	CLogCombo	m_cboBodies;
	//}}AFX_DATA

protected:
	virtual void InitObjectDlg();
	virtual void InitVariableDlg();
	void UpdateTempRadius();
	void UpdateTempVariable();


// operations
protected:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRadiusDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CRadiusDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_LABRADDLG_H__C2FF9B82_A84B_11D1_A09F_0000C0086DCF__INCLUDED_)
