#if !defined(AFX_RESISTANCEDLG_H__4B2F11E1_2483_11D6_8735_C9FB03B18D44__INCLUDED_)
#define AFX_RESISTANCEDLG_H__4B2F11E1_2483_11D6_8735_C9FB03B18D44__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ResistanceDlg.h : header file
//

#include "DrawObjDlg.h"

/////////////////////////////////////////////////////////////////////////////
// CResistanceDlg dialog

class CResistanceDlg : public CDrawObjDlg
{
// Construction
public:
	CResistanceDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CResistanceDlg)
	enum { IDD = IDD_RESISTANCE };
	CStatic	m_stcEquals;
	CButton	m_stcGiven;
	CStatic	m_stcOr;
	CLogBtn	m_btnUnknown;
	CLogEdit m_editValue;
	CButton	m_grpBox;
	CStatic	m_stcInstructions;
	CStatic	m_stcLet;
	CStatic		m_stcTimeList;
	CLogList	m_listBodies;
	CLogBtn		m_Ok;
	CLogBtn		m_Cancel;
	CLogCombo	m_cboTimeList;
	CLabelRichEdit	m_editName;
	//}}AFX_DATA

	// Same basic dialog for resistance or capacitance. 
	BOOL m_bCapacitance;		// T => capacitance; F = resistance

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CResistanceDlg)
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
	//{{AFX_MSG(CResistanceDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnCheckUnknown();
	afx_msg void OnChangeGivenValue();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

class CCapacitanceDlg : public CResistanceDlg
{
// Construction
public:
	CCapacitanceDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_RESISTANCEDLG_H__4B2F11E1_2483_11D6_8735_C9FB03B18D44__INCLUDED_)
