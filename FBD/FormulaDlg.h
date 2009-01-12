#if !defined(AFX_FORMULADLG_H__91138681_35BE_11D2_B9ED_0000C5465DC1__INCLUDED_)
#define AFX_FORMULADLG_H__91138681_35BE_11D2_B9ED_0000C5465DC1__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// FormulaDlg.h : header file
//
#include "EQEdit.h"
/////////////////////////////////////////////////////////////////////////////
// CFormulaDlg dialog
#include "LogEdit.h"
#include "Lgdialog.h"

typedef struct
{
	char* strName;
	char* strForm;
	char* strHelp;
} formString;

extern const formString Formulas[];
extern const int numForms;

class CFormulaDlg : public CLogDialog
{
// Construction
public:
	CFormulaDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CFormulaDlg)
	enum { IDD = IDD_FORMULA };
	CLogBtn	m_Ok;
	CLogBtn	m_Cancel;
	CLogList	m_listFormulaName;
	//}}AFX_DATA
	CString m_strFormula;
	CHintRichEdit m_editFormula;


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CFormulaDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CFormulaDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSelchangeFormula();
	virtual void OnOK();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_FORMULADLG_H__91138681_35BE_11D2_B9ED_0000C5465DC1__INCLUDED_)
