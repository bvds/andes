#if !defined(AFX_RELVELDLG_H__9BC5528E_FA27_4D8B_86FB_25907928F83D__INCLUDED_)
#define AFX_RELVELDLG_H__9BC5528E_FA27_4D8B_86FB_25907928F83D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// RelVelDlg.h : header file
//

#include "DrawObjDlg.h"

/////////////////////////////////////////////////////////////////////////////
// CRelVelDlg dialog

class CRelVelDlg : public CDrawObjDlg
{
// Construction
public:
	CRelVelDlg(CDrawObj* pObj, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CRelVelDlg)
	enum { IDD = IDD_VECTOR_RELVEL };
	CStatic	m_stcVecAng2;
	CStatic	m_stcVecAng1;
	CStatic	m_stcVecAng;
	CStatic	m_stcLet;
	CLogCombo	m_cboBody;
	CLogCombo	m_cboFrom;
	CStatic		m_stcTimeList;
	CLogCombo	m_cboTimeList;
	CSpinButtonCtrl	m_spinDir;
	CLogEdit	m_editOrientation;
	CLogCombo	m_cboZDir;
	CLabelRichEdit	m_editName;
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CRelVelDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_VIRTUAL
	DECLARE_CTL_TBL()

	virtual void InitObjectDlg();
	void UpdateTempVector();
	virtual void InitVariableDlg();
	void UpdateTempVariable();

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CRelVelDlg)
	afx_msg void OnSelchangeZdir();
	afx_msg void OnSelchangeBody();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_RELVELDLG_H__9BC5528E_FA27_4D8B_86FB_25907928F83D__INCLUDED_)
