#if !defined(AFX_VALUEDLG_H__2A9DC953_EB23_424E_86C2_406EB8F4F5E2__INCLUDED_)
#define AFX_VALUEDLG_H__2A9DC953_EB23_424E_86C2_406EB8F4F5E2__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "LogEdit.h"
#include "EQEdit.h"
#include "RichCombo.h"			// for rich static label
#include "fbdobj.h"

// ValueDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CValueDlg dialog

class CValueDlg : public CLogDialog
{
// Construction
public:
	void TransferValues(BOOL bSaving);
	void OnUpdateName(CString strName);
	CValueDlg(CVector* pVec, CWnd* pParent = NULL);   // constructor

// Dialog Data
	//{{AFX_DATA(CValueDlg)
	enum { IDD = IDD_VECTOR_VALUES };
	CStatic	m_stcAngIcon;
	CSpinButtonCtrl	m_spinDirection;
	CRichStatic	m_stcDirLabel;
	CRichStatic	m_stcMagLabel;
	CRichStatic	m_stcZCLabel;
	CRichStatic	m_stcYCLabel;
	CRichStatic	m_stcXCLabel;
	CLogCombo	m_cboZDir;
	CLogEdit	m_editDirValue;
	CLogBtn	m_btnMagDir;
	CEQRichEdit	m_editZCValue;
	CEQRichEdit	m_editYCValue;
	CEQRichEdit	m_editXCValue;
	CEQRichEdit	m_editMagValue;
	CLogBtn	m_btnZCUnknown;
	CLogBtn	m_btnYCUnknown;
	CLogBtn	m_btnXCUnknown;
	CLogBtn	m_btnMagUnknown;
	CLogBtn	m_btnDirUnknown;
	int		m_bCompoForm;
	//}}AFX_DATA
	CVector* m_pVec;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CValueDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual LRESULT DefWindowProc(UINT message, WPARAM wParam, LPARAM lParam);
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CValueDlg)
	afx_msg void OnCompoBtn();
	afx_msg void OnMagdirBtn();
	afx_msg void OnCheckMagUnknown();
	afx_msg void OnCheckDirUnknown();
	afx_msg void OnCheckXCUnknown();
	afx_msg void OnCheckYCUnknown();
	afx_msg void OnCheckZCUnknown();
	afx_msg void OnChangeMagValue();
	afx_msg void OnChangeDirValue();
	afx_msg void OnChangeXCValue();
	afx_msg void OnChangeYCValue();
	afx_msg void OnChangeZCValue();
	virtual BOOL OnInitDialog();
	afx_msg void OnSelchangeZdir();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VALUEDLG_H__2A9DC953_EB23_424E_86C2_406EB8F4F5E2__INCLUDED_)
