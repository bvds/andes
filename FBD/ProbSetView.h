#if !defined(AFX_PROBSETVIEW_H__0CBEBAC0_ECF3_11D2_807C_F206984FCFB0__INCLUDED_)
#define AFX_PROBSETVIEW_H__0CBEBAC0_ECF3_11D2_807C_F206984FCFB0__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// ProbSetView.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CProbSetView form view

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

#include "rectdlg.h"	// for CPreviewStc

class CProbSetView : public CFormView
{
protected:
	CProbSetView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CProbSetView)

// Form Data
public:
	//{{AFX_DATA(CProbSetView)
	enum { IDD = IDD_PROBSET_VIEW };
	CPreviewStc	m_stcPreview;
	CListBox	m_lstProblems;
	//}}AFX_DATA

// Attributes
public:
	CProblemSet* GetDocument();
	CString  m_strSelectedProb;     // id of last selected problem, empty if none

// Operations
public:
	void ShowVideo(CTask* pTask);
	BOOL m_wDefaultHelpFlags;
	CString GetProblemFilePath(LPCTSTR pszProblemId);
	BOOL m_bInitialized;
	CString GetSelectedProb();
	void UpdateProblemList();
	CFBDDoc* m_pTempDoc;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CProbSetView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	virtual void OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView);
	//}}AFX_VIRTUAL

// Implementation
protected:
	void SetPreview(LPCTSTR pszProblemName);
	virtual ~CProbSetView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CProbSetView)
	afx_msg void OnSelchangeProblemList();
	afx_msg void OnOpenProblem();
	afx_msg void OnDeleteSolution();
	afx_msg void OnUpdateOpenProblem(CCmdUI* pCmdUI);
	afx_msg void OnUpdateDeleteSolution(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in ProbSetView.cpp
inline CProblemSet* CProbSetView::GetDocument()
   { return (CProblemSet*)m_pDocument; }
#endif
/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PROBSETVIEW_H__0CBEBAC0_ECF3_11D2_807C_F206984FCFB0__INCLUDED_)
