#if !defined(AFX_STARTPG_H__5201E77A_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_STARTPG_H__5201E77A_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// StartPg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CStartPg dialog

class CStartPg : public CPropertyPage
{
	DECLARE_DYNCREATE(CStartPg)

// Construction
public:
	CStartPg();
	~CStartPg();

// Dialog Data
	//{{AFX_DATA(CStartPg)
	enum { IDD = IDD_WELCOME_DLG };
	CStatic	m_stcVersion;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CStartPg)
	public:
	virtual BOOL OnSetActive();
	virtual LRESULT OnWizardNext();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CStartPg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STARTPG_H__5201E77A_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
