#if !defined(AFX_VARIABLEDLG_H__4655AFE1_1004_11D2_B9ED_0000C5465DC1__INCLUDED_)
#define AFX_VARIABLEDLG_H__4655AFE1_1004_11D2_B9ED_0000C5465DC1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// VariableDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CVariableDlg dialog
#include "DrawObjDlg.h"
#include "EQEdit.h"			// for rich edit label of angle


class CVariableDlg : public CDrawObjDlg
{
// Construction
public:
	CVariableDlg(CDrawObj* pObj = NULL, CWnd* pParent = NULL);   // standard constructor

	DECLARE_CTL_TBL()
	virtual CLabelRichEdit* GetLabelCtrl();

// Dialog Data
	//{{AFX_DATA(CVariableDlg)
	enum { IDD = IDD_DECLARE_VARIABLE };
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CStatic	m_stcLet;
	CStatic	m_stcBody;
	CStatic	m_stcDueTo;
	CStatic	m_stcTime;
	CLogCombo	m_cboTime;
	CLabelRichEdit	m_editName;
	CLogCombo	m_cboBody;
	CLogCombo	m_cboAgent;
	CStatic	m_stcValue;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVariableDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

public:
	CString GetSpec(); 
	// helpers for parsing dialog specs:
	BOOL UsesSlot(const CString strSpec, const CString& strSlot);
	CString GetSlotChoices(const CString& strSpec, const CString& strSlot);
	CString GetSlotLabel(const CString& strSpec, const CString& strSlot);
	
// Implementation
protected:
	virtual void InitVariableDlg();

	void BuildVariable();
	void UpdateTempVariable();
	void SwitchTimeAgent();

	CString LookupCtrlName(int nID);


	// Generated message map functions
	//{{AFX_MSG(CVariableDlg)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#define NETFORCE		8
/////////////////////////////////////////////////////////////////////////////
// CVarTypeDlg dialog

class CVarTypeDlg : public CDialog
{
// Construction
public:
	CVarTypeDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CVarTypeDlg)
	enum { IDD = IDD_VARTYPE };
	int		m_nType;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CVarTypeDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CVarTypeDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_VARIABLEDLG_H__4655AFE1_1004_11D2_B9ED_0000C5465DC1__INCLUDED_)
