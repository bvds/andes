#if !defined(AFX_LAWDIALOG_H__F852DAE1_CFDC_11D2_B9ED_A3BA18F40054__INCLUDED_)
#define AFX_LAWDIALOG_H__F852DAE1_CFDC_11D2_B9ED_A3BA18F40054__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// LawDialog.h : header file
//
#include "LogEdit.h"

/////////////////////////////////////////////////////////////////////////////
// CLawDialog dialog

class CLawDialog : public CCheckedDlg
{
// Construction
public:
	CLawDialog(CPrincObj* pPrinc = NULL, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CLawDialog)
	enum { IDD = IDD_LAW_APPLICATION };
	CLogBtn	m_btnCancel;
	CLogBtn	m_btnOK;
	CLogList	m_lbBodies;
	CLogList	m_lbPrinciples;
	CString		m_strPrinciple;
	//}}AFX_DATA
	CStringList	m_listBodies;	// list of selected body strings

	CPrincObj* m_pPrinc;

	// instead of cloning a principle (which contains a stage), we use
	// little helper type for holding defining attributes & state of a principle
	// to remember original and last defined state 
	// can assign and compare to princobjs
	struct CPrincDef {
		CString m_strLaw;
		CString m_strBodies; // as delimited string for easy assignment, comparison
		Status	m_status;

		const CPrincDef& operator=(const CPrincObj& princ) {
			m_strLaw	= princ.m_strLaw;
			m_strBodies = princ.GetBodyStr();
			m_status	= princ.m_status;
			return *this;
		};
		BOOL operator==(const CPrincObj& princ) {
			CString strPrincBodies = princ.GetBodyStr();
			return m_strLaw		== princ.m_strLaw && 
				   m_strBodies	== strPrincBodies &&
				   m_status		== princ.m_status;
		};
		void CopyTo(CPrincObj& princ) { // transfer def to given princ (for revert)
			princ.SetLaw(m_strLaw);
			// unpack body string into string list
			CStringList listBodies;
			SplitStr(m_strBodies, listBodies);
			princ.SetBodies(listBodies);
			princ.m_status = m_status;
		};
	};
	CPrincDef m_defOrig, m_defLast; // saves original, last applied state

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CLawDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL
	virtual void OnCancel();

	DECLARE_CTL_TBL()

// Implementation
protected:
	void FillLawList();
	BOOL CheckDialog();

	// Generated message map functions
	//{{AFX_MSG(CLawDialog)
	virtual BOOL OnInitDialog();
	virtual void OnOK();
	afx_msg void OnDialogWhatswrong();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_LAWDIALOG_H__F852DAE1_CFDC_11D2_B9ED_A3BA18F40054__INCLUDED_)
