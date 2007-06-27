#if !defined(AFX_TIMECONSTANTDLG_H__14A29F6C_2861_4EF1_8780_B1A32FE1298D__INCLUDED_)
#define AFX_TIMECONSTANTDLG_H__14A29F6C_2861_4EF1_8780_B1A32FE1298D__INCLUDED_


#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// TimeConstantDlg.h : header file
//

#include "DrawObjDlg.h"
#include "EQEdit.h"

/////////////////////////////////////////////////////////////////////////////
// CTimeConstantDlg dialog

class CTimeConstantDlg : public CDrawObjDlg
{
// Construction
public:
	CTimeConstantDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CTimeConstantDlg)
	enum { IDD = IDD_TIMECONSTANT };
	CStatic	m_stcEquals;
	CButton	m_stcGiven;
	CStatic	m_stcOr;
	CLogBtn	m_btnUnknown;
	CEQRichEdit m_editValue;
	CStatic	m_stcInstructions;
	CStatic	m_stcLet;
	CLogList	m_listBodies;
	CLogBtn		m_Ok;
	CLogBtn		m_Cancel;
	CLabelRichEdit	m_editName;
	//}}AFX_DATA

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTimeConstantDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnOK();
	//}}AFX_VIRTUAL

	// for CDrawObjDlg property transfer protocol:
	virtual void InitVariableDlg();
	virtual void UpdateTempVariable();

	virtual CLabelRichEdit* GetLabelCtrl() { return &m_editName; };

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CTimeConstantDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnCheckUnknown();
	afx_msg void OnChangeGivenValue();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.


#endif // !defined(AFX_TIMECONSTANTDLG_H__14A29F6C_2861_4EF1_8780_B1A32FE1298D__INCLUDED_)
