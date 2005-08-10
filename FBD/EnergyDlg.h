#if !defined(AFX_ENERGYDLG_H__6E2D94C1_23DA_11D3_B260_0000C5465DC1__INCLUDED_)
#define AFX_ENERGYDLG_H__6E2D94C1_23DA_11D3_B260_0000C5465DC1__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// EnergyDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CEnergyDlg dialog
#include "DrawObjDlg.h"

class CEnergyDlg : public CDrawObjDlg
{
// Construction
public:
	virtual CLabelRichEdit* GetLabelCtrl();
	CEnergyDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	DECLARE_DYNAMIC(CEnergyDlg);

	DECLARE_CTL_TBL()

// Dialog Data
	//{{AFX_DATA(CEnergyDlg)
	enum { IDD = IDD_ENERGY };
	CStatic	m_stcEquals;
	CButton	m_stcGiven;
	CStatic	m_stcOr;
	CLogBtn	m_btnUnknown;
	CLogEdit m_editValue;
	CStatic	m_stcLet;
	CStatic	m_stcPE;
	CLogBtn	m_btnOk;
	CLogBtn	m_btnCancel;
	CListBox	m_lstPrefix;
	CStatic	m_stcAgent;
	CStatic	m_stcTime;
	CLogCombo	m_cboBody;
	CStatic	m_stcValue;
	CLabelRichEdit	m_editName;
	CLogCombo	m_cboTime;
	CLogCombo	m_cboEnergyType;
	CLogCombo	m_cboAgent;
	//}}AFX_DATA
protected:
	virtual void InitVariableDlg();

	// new: NB!: order must parallel order of choices in combo box (not alphabetically sorted)
    enum EnergyType {TotalMechanical, Kinetic, RotationalKinetic, Gravitational, Elastic, Electric};


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEnergyDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	void UpdateTempVariable();

	// Generated message map functions
	//{{AFX_MSG(CEnergyDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnSelchangeNrgType();
	afx_msg void OnCheckUnknown();
	afx_msg void OnChangeGivenValue();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_ENERGYDLG_H__6E2D94C1_23DA_11D3_B260_0000C5465DC1__INCLUDED_)
