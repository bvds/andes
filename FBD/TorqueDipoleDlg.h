#if !defined(AFX_TORQUEDIPOLEDLG_H__EC2F5A70_D0AC_4A16_B9BC_DF162092CB2C__INCLUDED_)
#define AFX_TORQUEDIPOLEDLG_H__EC2F5A70_D0AC_4A16_B9BC_DF162092CB2C__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// TorqueDlg.h : header file
//
#include "DrawObjDlg.h"
#include "RichCombo.h"
#include "ValueDlg.h"

/////////////////////////////////////////////////////////////////////////////
// CTorqueDipoleDlg dialog

class CTorqueDipoleDlg : public CDrawObjDlg
{
// Construction
public:
	CTorqueDipoleDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CTorqueDipoleDlg)
	enum { IDD = IDD_TORQUE_DIPOLE };
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
	CLogCombo	m_cboField;
	CLogCombo	m_cboBody;
	//}}AFX_DATA
	CValueDlg* m_pDlgValues;


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTorqueDipoleDlg)
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
	//{{AFX_MSG(CTorqueDipoleDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnSelchangeZdir();
	afx_msg void OnChangeVectorNameText();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.


#endif // !defined(AFX_TORQUEDIPOLEDLG_H__EC2F5A70_D0AC_4A16_B9BC_DF162092CB2C__INCLUDED_)
