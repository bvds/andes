#if !defined(AFX_FINISHPG_H__5201E779_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_FINISHPG_H__5201E779_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// FinishPg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CFinishPg dialog

class CFinishPg : public CPropertyPage
{
	DECLARE_DYNCREATE(CFinishPg)

// Construction
public:
	CFinishPg();
	~CFinishPg();

// Dialog Data
	//{{AFX_DATA(CFinishPg)
	enum { IDD = IDD_FINISH_DLG };
		// NOTE - ClassWizard will add data members here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_DATA

// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CFinishPg)
	public:
	virtual BOOL OnSetActive();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CFinishPg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_FINISHPG_H__5201E779_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
