#if !defined(AFX_IMPULSEDLG_H__25EAFEC8_ABEF_477E_A375_34B5238F6AA5__INCLUDED_)
#define AFX_IMPULSEDLG_H__25EAFEC8_ABEF_477E_A375_34B5238F6AA5__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ImpulseDlg.h : header file
//

#include "DrawObjDlg.h"

/////////////////////////////////////////////////////////////////////////////
// CImpulseDlg dialog

class CImpulseDlg : public CDrawObjDlg
{
// Construction
public:
	CImpulseDlg(CDrawObj* pObj, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CImpulseDlg)
	enum { IDD = IDD_VECTOR_IMPULSE };
	CStatic	m_stcVecAng2;
	CStatic	m_stcVecAng1;
	CStatic	m_stcVecAng;
	CStatic	m_stcLet;
	CLogCombo	m_cboBody;
	CLogCombo	m_cboDueTo;
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
	//{{AFX_VIRTUAL(CImpulseDlg)
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
	//{{AFX_MSG(CImpulseDlg)
	afx_msg void OnSelchangeZdir();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_IMPULSEDLG_H__25EAFEC8_ABEF_477E_A375_34B5238F6AA5__INCLUDED_)
