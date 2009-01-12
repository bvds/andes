#if !defined(AFX_INSTDIRPG_H__5201E777_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
#define AFX_INSTDIRPG_H__5201E777_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// InstDirPg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CInstDirPg dialog

class CInstDirPg : public CPropertyPage
{
	DECLARE_DYNCREATE(CInstDirPg)

// Construction
public:
	int m_nFiles;
	CInstDirPg();
	~CInstDirPg();

// Dialog Data
	//{{AFX_DATA(CInstDirPg)
	enum { IDD = IDD_DESTINATION_DLG };
	CStatic	m_ctrlInstDir;
	CString	m_strInstDir;
	//}}AFX_DATA


// Overrides
	// ClassWizard generate virtual function overrides
	//{{AFX_VIRTUAL(CInstDirPg)
	public:
	virtual LRESULT OnWizardNext();
	virtual BOOL OnSetActive();
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	// Generated message map functions
	//{{AFX_MSG(CInstDirPg)
	afx_msg void OnBrowse();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_INSTDIRPG_H__5201E777_3B1A_11D1_A09F_0000C0086DCF__INCLUDED_)
