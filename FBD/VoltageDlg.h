#if !defined(AFX_VOLTAGEDLG_H__5506F300_240A_11D6_8735_FCB98C456A47__INCLUDED_)
#define AFX_VOLTAGEDLG_H__5506F300_240A_11D6_8735_FCB98C456A47__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// VoltageDlg.h : header file
//

#include "DrawObjDlg.h"
#include "EQEdit.h"

/////////////////////////////////////////////////////////////////////////////
// CVoltageDlg dialog

class CVoltageDlg : public CDrawObjDlg
{
// Construction
public:
	CVoltageDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CVoltageDlg)
	enum { IDD = IDD_VOLTAGE };
	CStatic	m_stcEquals;
	CButton	m_stcGiven;
	CStatic	m_stcOr;
	CLogBtn	m_btnUnknown;
	CEQRichEdit m_editValue;
	CStatic	m_stcLet;
	CLogCombo	m_cboTime;
	CLogCombo	m_cboComponent;
	CLabelRichEdit	m_editName;
	CLogBtn	m_btnCancel;
	CLogBtn	m_btnOk;
	int		m_nType;
	//}}AFX_DATA
	DECLARE_CTL_TBL()

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVoltageDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnOK();
	//}}AFX_VIRTUAL

	// for CDrawObjDlg property transfer protocol:
	virtual void InitVariableDlg();
	virtual void UpdateTempVariable();

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CVoltageDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnCheckUnknown();
	afx_msg void OnChangeGivenValue();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VOLTAGEDLG_H__5506F300_240A_11D6_8735_FCB98C456A47__INCLUDED_)
