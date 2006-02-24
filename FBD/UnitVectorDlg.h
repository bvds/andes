#if !defined(AFX_UNITVECTORDLG_H__EEEEEF15_EF3F_4F7C_A608_0130015A56E6__INCLUDED_)
#define AFX_UNITVECTORDLG_H__EEEEEF15_EF3F_4F7C_A608_0130015A56E6__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// UnitVectorDlg.h : header file
//

#include "DrawObjDlg.h"

/////////////////////////////////////////////////////////////////////////////
// CUnitVectorDlg dialog

class CUnitVectorDlg : public CDrawObjDlg
{
// Construction
public:
	CUnitVectorDlg(CDrawObj* pObj, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CUnitVectorDlg)
	enum { IDD = IDD_VECTOR_UNIT };
	CLogBtn	m_btnNormal;
	CLogBtn  m_btnAt;
	CLogCombo	m_cboTowardsAway;
	CStatic	m_stcVecAng2;
	CStatic	m_stcVecAng1;
	CStatic	m_stcVecAng;
	CStatic	m_stcLet;
	CLogCombo	m_cboBody;
	CLogCombo	m_cboAtBody;
	CLogCombo	m_cboAgent;
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
	//{{AFX_VIRTUAL(CUnitVectorDlg)
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
	//{{AFX_MSG(CUnitVectorDlg)
	afx_msg void OnSelchangeZdir();
	afx_msg void OnNormalBtn();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_UNITVECTORDLG_H__EEEEEF15_EF3F_4F7C_A608_0130015A56E6__INCLUDED_)
