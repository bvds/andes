#if !defined(AFX_VECPOSDLG_H__81AFAA61_4A1C_11D4_807C_0000C546451F__INCLUDED_)
#define AFX_VECPOSDLG_H__81AFAA61_4A1C_11D4_807C_0000C546451F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// VecPosDlg.h : header file
//

#include "DrawObjDlg.h"

/////////////////////////////////////////////////////////////////////////////
// CVecPosDlg dialog

class CVecPosDlg : public CDrawObjDlg
{
// Construction
public:
	CVecPosDlg(CDrawObj* pObj, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CVecPosDlg)
	enum { IDD = IDD_VECTOR_POS };
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
	//{{AFX_VIRTUAL(CVecPosDlg)
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
	//{{AFX_MSG(CVecPosDlg)
	afx_msg void OnSelchangeZdir();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VECPOSDLG_H__81AFAA61_4A1C_11D4_807C_0000C546451F__INCLUDED_)
