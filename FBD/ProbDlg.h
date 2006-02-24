#if !defined(AFX_PROBDLG_H__DE49E621_B5CC_4255_AB5D_4346B75B8188__INCLUDED_)
#define AFX_PROBDLG_H__DE49E621_B5CC_4255_AB5D_4346B75B8188__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ProbDlg.h : header file
//
#include "DrawObjDlg.h"
/////////////////////////////////////////////////////////////////////////////
// CProbDlg dialog

class CProbDlg : public CDrawObjDlg
{
// Construction
public:
	CProbDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CProbDlg)
	enum { IDD = IDD_PROBABILITY };
	CStatic	m_stcEquals;
	CButton	m_stcGiven;
	CStatic	m_stcOr;
	CLogBtn	m_btnUnknown;
	// CEQRichEdit m_editValue;
	CLogEdit m_editValue;
	CLogBtn	m_btnCancel;
	CLogBtn	m_btnOk;
	CRichEditEx		m_editEvent;
	CLabelRichEdit	m_editName;
	//}}AFX_DATA

	DECLARE_DYNAMIC(CProbDlg);

	DECLARE_CTL_TBL()

	static void EventNameToHelpFormat(CString& strName);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CProbDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

	virtual CLabelRichEdit* GetLabelCtrl();

// Implementation
protected:
	virtual void InitVariableDlg();
	void UpdateTempVariable();
	CFont m_fontSymbol;

	// Generated message map functions
	//{{AFX_MSG(CProbDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnCheckUnknown();
	afx_msg void OnChangeGivenValue();
	//}}AFX_MSG
	afx_msg void OnInsertProbSymbol(UINT nID);
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PROBDLG_H__DE49E621_B5CC_4255_AB5D_4346B75B8188__INCLUDED_)
