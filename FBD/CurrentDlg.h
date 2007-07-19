#if !defined(AFX_CURRENTDLG_H__4B2F11E0_2483_11D6_8735_C9FB03B18D44__INCLUDED_)
#define AFX_CURRENTDLG_H__4B2F11E0_2483_11D6_8735_C9FB03B18D44__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// CurrentDlg.h : header file
//

#include "DrawObjDlg.h"
#include "EQEdit.h"

/////////////////////////////////////////////////////////////////////////////
// CCurrentDlg dialog

class CCurrentDlg : public CDrawObjDlg
{
// Construction
public:
	CCurrentDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CCurrentDlg)
	enum { IDD = IDD_CURRENT };
	CLogList	m_listBodies;
	CStatic	m_stcEquals;
	CButton	m_stcGiven;
	CStatic	m_stcOr;
	CLogBtn	m_btnUnknown;
	CEQRichEdit m_editValue;
	CStatic	m_stcLet;
	CLogCombo	m_cboTime;
	CLabelRichEdit	m_editName;
	CLogBtn	m_btnOK;
	CLogBtn	m_btnCancel;
	//}}AFX_DATA

	DECLARE_CTL_TBL()

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CCurrentDlg)
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
	//{{AFX_MSG(CCurrentDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnUpdateType();
	afx_msg void OnCheckUnknown();
	afx_msg void OnChangeGivenValue();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CURRENTDLG_H__4B2F11E0_2483_11D6_8735_C9FB03B18D44__INCLUDED_)
