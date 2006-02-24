#if !defined(AFX_DIPOLEDLG_H__441AC590_2389_4425_9479_332D2F33ACE7__INCLUDED_)
#define AFX_DIPOLEDLG_H__441AC590_2389_4425_9479_332D2F33ACE7__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// DipoleDlg.h : header file
//
#include "DrawObjDlg.h"


/////////////////////////////////////////////////////////////////////////////
// CDipoleDlg dialog

class CDipoleDlg : public CDrawObjDlg
{
// Construction
public:
	CDipoleDlg(CDrawObj* pObj, BOOL m_bMagnetic = FALSE, CWnd* pParent = NULL);   


// Dialog Data
	//{{AFX_DATA(CDipoleDlg)
	enum { IDD = IDD_VECTOR_DIPOLE };
	CStatic	m_stcType;
	CLogCombo	m_cboBody;
	CStatic	    m_stcTimeList;
	CLogCombo	m_cboTimeList;
	CStatic	    m_stcVecAng2;
	CLogEdit	m_editOrientation;
	CSpinButtonCtrl	m_spinDir;
	CStatic	    m_stcVecAng1;
	CStatic	    m_stcVecAng;
	CLogCombo	m_cboZDir;
	CStatic	    m_stcLet;
	CLabelRichEdit	m_editName;
	CLogBtn	    m_Ok;
	CLogBtn	    m_Cancel;
	//}}AFX_DATA

	BOOL m_bMagnetic;		// set if this is magnetic field

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDipoleDlg)
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
	//{{AFX_MSG(CDipoleDlg)
		afx_msg void OnSelchangeZdir();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_DIPOLEDLG_H__441AC590_2389_4425_9479_332D2F33ACE7__INCLUDED_)
