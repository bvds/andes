#if !defined(AFX_PROBSETVIEW_H__1075C002_DDE4_11D2_807C_8F49523D0910__INCLUDED_)
#define AFX_PROBSETVIEW_H__1075C002_DDE4_11D2_807C_8F49523D0910__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ProbSetEditView.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CProbSetEditView form view

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

class CProbSetEditView : public CFormView
{
protected:
	CProbSetEditView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CProbSetEditView)

// Form Data
public:
	//{{AFX_DATA(CProbSetEditView)
	enum { IDD = IDD_PROBSETEDITVIEW };
	CStatic	m_stcSelected;
	CComboBox	m_cboValue;
	CListBox	m_lstProblems;
	CListBox	m_lstOptions;
	//}}AFX_DATA

// Attributes
public:
	CProblemSet* GetDocument();

// Operations
public:
	CString GetOptionVal(COptionSet* pOpts, CString strName, int *pWhere = NULL);
	BOOL GetSelectedOption(CString& strName);
	void FillProblemList();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CProbSetEditView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CString GetSelectedProb();
	COptionSet* GetSelectedOptions();
	void FillOptionList(COptionSet* pOptionSet);
	
	virtual ~CProbSetEditView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CProbSetEditView)
	afx_msg void OnAddOption();
	afx_msg void OnAddProblem();
	afx_msg void OnSelchangeProblemList();
	afx_msg void OnProbsetDeleteProb();
	afx_msg void OnProbsetDeleteOption();
	afx_msg void OnSelchangeOptionList();
	afx_msg void OnScaffoldCoached();
	afx_msg void OnScaffoldIntro();
	afx_msg void OnScaffoldSolo();
	afx_msg void OnProbsetMovedown();
	afx_msg void OnProbsetMoveup();
	afx_msg void OnUpdateMovedown(CCmdUI* pCmdUI);
	afx_msg void OnUpdateMoveup(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg LRESULT OnIdleUpdateCmdUI(WPARAM wParam, LPARAM lParam);
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in ProbSetView.cpp
inline CProblemSet* CProbSetEditView::GetDocument()
   { return (CProblemSet*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PROBSETVIEW_H__1075C002_DDE4_11D2_807C_8F49523D0910__INCLUDED_)
