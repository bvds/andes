#if !defined(AFX_OLIVIEW_H__36349689_0A20_4DAE_8093_B557ABD83201__INCLUDED_)
#define AFX_OLIVIEW_H__36349689_0A20_4DAE_8093_B557ABD83201__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// OliView.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// COliView form view

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

class COliView : public CFormView
{
protected:
	COliView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(COliView)

// Form Data
public:
	//{{AFX_DATA(COliView)
	enum { IDD = IDD_OLIVIEW };
	CStatic	m_stcStatus;
	CString	m_strTaskName;
	//}}AFX_DATA

// Attributes
public:
	CProblemSet* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COliView)
	public:
	virtual void OnInitialUpdate();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnUpdate(CView* pSender, LPARAM lHint, CObject* pHint);
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~COliView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(COliView)
			afx_msg void OnOpenProblem();
	afx_msg void OnTimer(UINT nIDEvent);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	BOOL m_bInitialized;
};

#ifndef _DEBUG  // debug version in ProbSetView.cpp
inline CProblemSet* COliView::GetDocument()
   { return (CProblemSet*)m_pDocument; }
#endif
/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_OLIVIEW_H__36349689_0A20_4DAE_8093_B557ABD83201__INCLUDED_)
