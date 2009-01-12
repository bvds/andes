#if !defined(AFX_TORQUEDLG_H__81AFAA60_4A1C_11D4_807C_0000C546451F__INCLUDED_)
#define AFX_TORQUEDLG_H__81AFAA60_4A1C_11D4_807C_0000C546451F__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// TorqueDlg.h : header file
//
#include "DrawObjDlg.h"
#include "RichCombo.h"
#include "ValueDlg.h"

/////////////////////////////////////////////////////////////////////////////
// CTorqueDlg dialog

class CTorqueDlg : public CDrawObjDlg
{
// Construction
public:
	CTorqueDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CTorqueDlg)
	enum { IDD = IDD_TORQUE };
	CStatic	m_stcLet;
	CStatic	m_stcVecAng1;
	CStatic	m_stcVecAng;
	CRichStatic	m_stcComp;
	CRichStatic	m_stcVecAng2;
	CStatic	m_stcTimeList;
	CLogBtn	m_btnNet;
	CLogBtn m_btnForce;
	CLogCombo	m_cboTimeList;
	CEdit	m_editOrientation;
	CSpinButtonCtrl	m_spinDir;
	CLabelRichEdit	m_editName;
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CLogCombo	m_cboZDir;
	CLogCombo	m_cboForcePt;
	CLogCombo	m_cboBody;
	CLogCombo	m_cboAgent;
	//}}AFX_DATA
	CValueDlg* m_pDlgValues;


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTorqueDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
	DECLARE_CTL_TBL()

	virtual void InitObjectDlg();
	virtual void InitVariableDlg();
	void UpdateTempVector();
	void UpdateTempVariable();
	
// Implementation
protected:
	void UpdateComponents();

	// Generated message map functions
	//{{AFX_MSG(CTorqueDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnSelchangeZdir();
	afx_msg void OnUpdateNet();
	afx_msg void OnChangeVectorNameText();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TORQUEDLG_H__81AFAA60_4A1C_11D4_807C_0000C546451F__INCLUDED_)
