#if !defined(AFX_LINEDLG_H__86075A61_B978_4D47_A730_48FABB002DBD__INCLUDED_)
#define AFX_LINEDLG_H__86075A61_B978_4D47_A730_48FABB002DBD__INCLUDED_

#
#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// LineDlg.h : header file
//
#include "DrawObjDlg.h"


/////////////////////////////////////////////////////////////////////////////
// CLineDlg dialog

class CLineDlg : public CDrawObjDlg
{
// Construction
public:
	CLineDlg(CDrawObj* pObj,  CWnd* pParent = NULL);   


// Dialog Data
	//{{AFX_DATA(CLineDlg)
	enum { IDD = IDD_LINE};
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

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLineDlg)
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
	//{{AFX_MSG(CLineDlg)
		afx_msg void OnSelchangeZdir();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.
#endif // !defined(AFX_LINEDLG_H__86075A61_B978_4D47_A730_48FABB002DBD__INCLUDED_)
